//! Resolve source identifiers and structure tags to unique names.
#![allow(non_snake_case)]

use std::collections::BTreeMap;
use super::{Ast::*, Types::Type, UniqueIds};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResolvedVar { pub unique_name: String, pub from_current_scope: bool, pub has_linkage: bool }
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResolvedStruct { pub unique_tag: String, pub struct_from_current_scope: bool }
type IdMap = BTreeMap<String, ResolvedVar>;
type StructMap = BTreeMap<String, ResolvedStruct>;

fn copied_ids(map: &IdMap) -> IdMap { map.iter().map(|(k,v)| (k.clone(), ResolvedVar { from_current_scope:false, ..v.clone() })).collect() }
fn copied_structs(map: &StructMap) -> StructMap { map.iter().map(|(k,v)| (k.clone(), ResolvedStruct { struct_from_current_scope:false, ..v.clone() })).collect() }

pub fn resolveType(structs: &StructMap, ty: Type) -> Type { match ty {
    Type::Struct(tag) => Type::Struct(structs.get(&tag).unwrap_or_else(|| panic!("specified undeclared structure type")).unique_tag.clone()),
    Type::Pointer(t) => Type::Pointer(Box::new(resolveType(structs, *t))),
    Type::Array(t,n) => Type::Array(Box::new(resolveType(structs,*t)),n),
    Type::Function(ps,r) => Type::Function(ps.into_iter().map(|t|resolveType(structs,t)).collect(), Box::new(resolveType(structs,*r))),
    t => t,
} }
fn exp(structs:&StructMap, ids:&IdMap, e:Exp)->Exp { match e {
    Exp::Var(v)=>Exp::Var(ids.get(&v).unwrap_or_else(||panic!("Undeclared variable {v}")).unique_name.clone()),
    Exp::Assignment(a,b)=>Exp::Assignment(Box::new(exp(structs,ids,*a)),Box::new(exp(structs,ids,*b))),
    Exp::Cast(t,e)=>Exp::Cast(resolveType(structs,t),Box::new(exp(structs,ids,*e))),
    Exp::Unary(o,e)=>Exp::Unary(o,Box::new(exp(structs,ids,*e))),
    Exp::Binary(o,a,b)=>Exp::Binary(o,Box::new(exp(structs,ids,*a)),Box::new(exp(structs,ids,*b))),
    Exp::Conditional(a,b,c)=>Exp::Conditional(Box::new(exp(structs,ids,*a)),Box::new(exp(structs,ids,*b)),Box::new(exp(structs,ids,*c))),
    Exp::FunCall(f,args)=>Exp::FunCall(ids.get(&f).unwrap_or_else(||panic!("Undeclared function!")).unique_name.clone(),args.into_iter().map(|e|exp(structs,ids,e)).collect()),
    Exp::Dereference(e)=>Exp::Dereference(Box::new(exp(structs,ids,*e))), Exp::AddrOf(e)=>Exp::AddrOf(Box::new(exp(structs,ids,*e))),
    Exp::Subscript(a,b)=>Exp::Subscript(Box::new(exp(structs,ids,*a)),Box::new(exp(structs,ids,*b))),
    Exp::SizeOf(e)=>Exp::SizeOf(Box::new(exp(structs,ids,*e))), Exp::SizeOfT(t)=>Exp::SizeOfT(resolveType(structs,t)),
    Exp::Dot(e,m)=>Exp::Dot(Box::new(exp(structs,ids,*e)),m), Exp::Arrow(e,m)=>Exp::Arrow(Box::new(exp(structs,ids,*e)),m),
    e => e,
} }
fn initializer(s:&StructMap,i:&IdMap,x:Initializer)->Initializer { match x { Initializer::SingleInit(e)=>Initializer::SingleInit(exp(s,i,e)), Initializer::CompoundInit(xs)=>Initializer::CompoundInit(xs.into_iter().map(|x|initializer(s,i,x)).collect()) } }
fn local_var(counter:usize,s:&StructMap,mut ids:IdMap,mut d:VariableDeclaration)->(usize,IdMap,VariableDeclaration) {
    if let Some(old)=ids.get(&d.name) { if old.from_current_scope && !(old.has_linkage && d.storageClass==Some(StorageClass::Extern)) { panic!("Duplicate variable declaration") } }
    let (counter,unique,linkage)=if d.storageClass==Some(StorageClass::Extern){(counter,d.name.clone(),true)}else{let(c,n)=UniqueIds::make_named_temporary(&d.name,counter);(c,n,false)};
    ids.insert(d.name.clone(),ResolvedVar{unique_name:unique.clone(),from_current_scope:true,has_linkage:linkage});
    d.name=unique; d.varType=resolveType(s,d.varType); d.init=d.init.map(|x|initializer(s,&ids,x)); (counter,ids,d)
}
fn function(counter:usize,s:&StructMap,mut ids:IdMap,mut f:FunctionDeclaration)->(usize,IdMap,FunctionDeclaration) {
    if matches!(ids.get(&f.name),Some(ResolvedVar{from_current_scope:true,has_linkage:false,..})){panic!("Duplicate declaration")}
    f.funType=resolveType(s,f.funType); ids.insert(f.name.clone(),ResolvedVar{unique_name:f.name.clone(),from_current_scope:true,has_linkage:true});
    let mut inner=copied_ids(&ids); let mut counter=counter; let mut params=Vec::new();
    for p in f.params { let dummy=VariableDeclaration{name:p,varType:Type::Int,init:None,storageClass:None}; let(c,m,d)=local_var(counter,s,inner,dummy);counter=c;inner=m;params.push(d.name); }
    f.params=params; f.body=f.body.map(|b|{let(c,b)=block(counter,&copied_structs(s),inner.clone(),b);counter=c;b}); (counter,ids,f)
}
fn structure(counter:usize,mut s:StructMap,mut d:StructDeclaration)->(usize,StructMap,StructDeclaration){
    let (counter,tag)=match s.get(&d.tag){Some(x) if x.struct_from_current_scope=>(counter,x.unique_tag.clone()),_=>{let(c,t)=UniqueIds::make_named_temporary(&d.tag,counter);s.insert(d.tag.clone(),ResolvedStruct{unique_tag:t.clone(),struct_from_current_scope:true});(c,t)}};
    d.tag=tag; for m in &mut d.members {m.memberType=resolveType(&s,m.memberType.clone())} (counter,s,d)
}
fn statement(counter:usize,s:&StructMap,ids:&IdMap,x:Statement)->(usize,Statement){match x{
    Statement::Return(e)=>(counter,Statement::Return(e.map(|e|exp(s,ids,e)))), Statement::Expression(e)=>(counter,Statement::Expression(exp(s,ids,e))),
    Statement::If(e,a,b)=>{let(c,a)=statement(counter,s,ids,*a);let(c,b)=match b{Some(b)=>{let(c,b)=statement(c,s,ids,*b);(c,Some(Box::new(b)))},None=>(c,None)};(c,Statement::If(exp(s,ids,e),Box::new(a),b))},
    Statement::While(e,b,id)=>{let(c,b)=statement(counter,s,ids,*b);(c,Statement::While(exp(s,ids,e),Box::new(b),id))},
    Statement::DoWhile(b,e,id)=>{let(c,b)=statement(counter,s,ids,*b);(c,Statement::DoWhile(Box::new(b),exp(s,ids,e),id))},
    Statement::For(init,cond,post,b,id)=>{let s1=copied_structs(s);let ids1=copied_ids(ids);let(c,ids2,init)=match init{ForInit::InitExp(e)=>(counter,ids1,ForInit::InitExp(e.map(|e|exp(&s1,ids,e)))),ForInit::InitDecl(d)=>{let(c,m,d)=local_var(counter,&s1,ids1,d);(c,m,ForInit::InitDecl(d))}};let(c,b)=statement(c,&s1,&ids2,*b);(c,Statement::For(init,cond.map(|e|exp(&s1,&ids2,e)),post.map(|e|exp(&s1,&ids2,e)),Box::new(b),id))},
    Statement::Compound(b)=>{let(c,b)=block(counter,&copied_structs(s),copied_ids(ids),b);(c,Statement::Compound(b))}, x=>(counter,x)}}
fn block(counter:usize,s:&StructMap,ids:IdMap,b:Block)->(usize,Block){let(mut c,mut sm,mut im)=(counter,s.clone(),ids);let mut out=Vec::new();for x in b.0{match x{BlockItem::Stmt(x)=>{let(n,x)=statement(c,&sm,&im,x);c=n;out.push(BlockItem::Stmt(x))},BlockItem::Decl(d)=>{let(n,ns,ni,d)=local_decl(c,sm,im,d);c=n;sm=ns;im=ni;out.push(BlockItem::Decl(d))}}}(c,Block(out))}
fn local_decl(c:usize,s:StructMap,i:IdMap,d:Declaration)->(usize,StructMap,IdMap,Declaration){match d{
    Declaration::VarDecl(d)=>{let(c,i,d)=local_var(c,&s,i,d);(c,s,i,Declaration::VarDecl(d))},
    Declaration::FunDecl(f) if f.body.is_some()=>panic!("nested function definitions are not allowed"),
    Declaration::FunDecl(f) if f.storageClass==Some(StorageClass::Static)=>panic!("static keyword not allowed on local function declarations"),
    Declaration::FunDecl(f)=>{let(c,i,f)=function(c,&s,i,f);(c,s,i,Declaration::FunDecl(f))},
    Declaration::StructDecl(d)=>{let(c,s,d)=structure(c,s,d);(c,s,i,Declaration::StructDecl(d))}}}
/// Port of `resolve`, threading the unique-id counter explicitly.
pub fn resolve(counter:usize,p:UntypedProgram)->(usize,UntypedProgram){let(mut c,mut s,mut i)=(counter,StructMap::new(),IdMap::new());let mut out=Vec::new();for d in p.0{match d{
    Declaration::FunDecl(f)=>{let(n,ni,f)=function(c,&s,i,f);c=n;i=ni;out.push(Declaration::FunDecl(f))},
    Declaration::VarDecl(mut d)=>{d.varType=resolveType(&s,d.varType);i.insert(d.name.clone(),ResolvedVar{unique_name:d.name.clone(),from_current_scope:true,has_linkage:true});out.push(Declaration::VarDecl(d))},
    Declaration::StructDecl(d)=>{let(n,ns,d)=structure(c,s,d);c=n;s=ns;out.push(Declaration::StructDecl(d))}}}(c,UntypedProgram(out))}
