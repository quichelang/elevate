#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustModule {
    pub items: Vec<RustItem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustItem {
    Struct(RustStruct),
    Enum(RustEnum),
    Function(RustFunction),
    Const(RustConst),
    Static(RustStatic),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustStruct {
    pub name: String,
    pub fields: Vec<RustField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustField {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustEnum {
    pub name: String,
    pub variants: Vec<RustVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustVariant {
    pub name: String,
    pub payload: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustFunction {
    pub name: String,
    pub params: Vec<RustParam>,
    pub return_type: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustParam {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustConst {
    pub name: String,
    pub ty: Option<String>,
    pub value: RustExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustStatic {
    pub name: String,
    pub ty: String,
    pub value: RustExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustExpr {
    Int(i64),
    Bool(bool),
    String(String),
    Var(String),
    Call {
        callee: String,
        args: Vec<RustExpr>,
    },
    Field {
        base: Box<RustExpr>,
        field: String,
    },
}
