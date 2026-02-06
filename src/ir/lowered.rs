#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustModule {
    pub items: Vec<RustItem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustItem {
    Use(RustUse),
    Struct(RustStruct),
    Enum(RustEnum),
    Function(RustFunction),
    Const(RustConst),
    Static(RustStatic),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustUse {
    pub path: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustStruct {
    pub is_public: bool,
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
    pub is_public: bool,
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
    pub is_public: bool,
    pub name: String,
    pub params: Vec<RustParam>,
    pub return_type: String,
    pub body: Vec<RustStmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustParam {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustStmt {
    Const(RustConst),
    Return(Option<RustExpr>),
    If {
        condition: RustExpr,
        then_body: Vec<RustStmt>,
        else_body: Option<Vec<RustStmt>>,
    },
    While {
        condition: RustExpr,
        body: Vec<RustStmt>,
    },
    Expr(RustExpr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustConst {
    pub is_public: bool,
    pub name: String,
    pub ty: String,
    pub value: RustExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustStatic {
    pub is_public: bool,
    pub name: String,
    pub ty: String,
    pub value: RustExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustExpr {
    Int(i64),
    Bool(bool),
    String(String),
    Path(Vec<String>),
    Call {
        callee: Box<RustExpr>,
        args: Vec<RustExpr>,
    },
    Field {
        base: Box<RustExpr>,
        field: String,
    },
    Match {
        scrutinee: Box<RustExpr>,
        arms: Vec<RustMatchArm>,
    },
    Try(Box<RustExpr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustMatchArm {
    pub pattern: RustPattern,
    pub value: RustExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustPattern {
    Wildcard,
    Variant {
        path: Vec<String>,
        binding: Option<String>,
    },
}
