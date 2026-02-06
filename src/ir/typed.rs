#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedModule {
    pub items: Vec<TypedItem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedItem {
    RustUse(TypedRustUse),
    Struct(TypedStruct),
    Enum(TypedEnum),
    Impl(TypedImpl),
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
    pub is_public: bool,
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
    pub is_public: bool,
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
    pub is_public: bool,
    pub name: String,
    pub params: Vec<TypedParam>,
    pub return_type: String,
    pub body: Vec<TypedStmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedImpl {
    pub target: String,
    pub methods: Vec<TypedFunction>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedParam {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedStmt {
    Const(TypedConst),
    DestructureConst {
        pattern: TypedDestructurePattern,
        value: TypedExpr,
    },
    Return(Option<TypedExpr>),
    If {
        condition: TypedExpr,
        then_body: Vec<TypedStmt>,
        else_body: Option<Vec<TypedStmt>>,
    },
    While {
        condition: TypedExpr,
        body: Vec<TypedStmt>,
    },
    Expr(TypedExpr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedConst {
    pub is_public: bool,
    pub name: String,
    pub ty: String,
    pub value: TypedExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedStatic {
    pub is_public: bool,
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
    Match {
        scrutinee: Box<TypedExpr>,
        arms: Vec<TypedMatchArm>,
    },
    Unary {
        op: TypedUnaryOp,
        expr: Box<TypedExpr>,
    },
    Binary {
        op: TypedBinaryOp,
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
    },
    Tuple(Vec<TypedExpr>),
    Range {
        start: Option<Box<TypedExpr>>,
        end: Option<Box<TypedExpr>>,
        inclusive: bool,
    },
    Try(Box<TypedExpr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedUnaryOp {
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedBinaryOp {
    And,
    Or,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedMatchArm {
    pub pattern: TypedPattern,
    pub value: TypedExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedPattern {
    Wildcard,
    Binding(String),
    Int(i64),
    Bool(bool),
    String(String),
    Tuple(Vec<TypedPattern>),
    Variant {
        path: Vec<String>,
        payload: Option<Box<TypedPattern>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedDestructurePattern {
    Name(String),
    Ignore,
    Tuple(Vec<TypedDestructurePattern>),
}
