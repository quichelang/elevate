#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedModule {
    pub items: Vec<TypedItem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedItem {
    Struct(TypedStruct),
    Enum(TypedEnum),
    Function(TypedFunction),
    Const(TypedConst),
    Static(TypedStatic),
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
    pub return_type: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedParam {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedConst {
    pub name: String,
    pub ty: Option<String>,
    pub value: TypedExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedStatic {
    pub name: String,
    pub ty: String,
    pub value: TypedExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedExpr {
    Int(i64),
    Bool(bool),
    String(String),
    Var(String),
    Call {
        callee: String,
        args: Vec<TypedExpr>,
    },
    Field {
        base: Box<TypedExpr>,
        field: String,
    },
}
