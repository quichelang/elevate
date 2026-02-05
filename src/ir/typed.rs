#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedModule {
    pub items: Vec<TypedItem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedItem {
    RustUse(TypedRustUse),
    Struct(TypedStruct),
    Enum(TypedEnum),
    Function(TypedFunction),
    Const(TypedConst),
    Static(TypedStatic),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedRustUse {
    pub path: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedStruct {
    pub name: String,
    pub fields: Vec<TypedField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedField {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedEnum {
    pub name: String,
    pub variants: Vec<TypedVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedVariant {
    pub name: String,
    pub payload: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedFunction {
    pub name: String,
    pub params: Vec<TypedParam>,
    pub return_type: String,
    pub body: Vec<TypedStmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedParam {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedStmt {
    Const(TypedConst),
    Return(Option<TypedExpr>),
    Expr(TypedExpr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedConst {
    pub name: String,
    pub ty: String,
    pub value: TypedExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedStatic {
    pub name: String,
    pub ty: String,
    pub value: TypedExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedExprKind {
    Int(i64),
    Bool(bool),
    String(String),
    Path(Vec<String>),
    Call {
        callee: Box<TypedExpr>,
        args: Vec<TypedExpr>,
    },
    Field {
        base: Box<TypedExpr>,
        field: String,
    },
    Try(Box<TypedExpr>),
}
