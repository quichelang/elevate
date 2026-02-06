#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustModule {
    pub items: Vec<RustItem>,
    pub ownership_notes: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustItem {
    Use(RustUse),
    Raw(String),
    Struct(RustStruct),
    Enum(RustEnum),
    Impl(RustImpl),
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
pub struct RustImpl {
    pub target: String,
    pub methods: Vec<RustFunction>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustParam {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustStmt {
    Const(RustConst),
    DestructureConst {
        pattern: RustDestructurePattern,
        value: RustExpr,
    },
    Assign {
        target: RustAssignTarget,
        op: RustAssignOp,
        value: RustExpr,
    },
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
    For {
        binding: RustDestructurePattern,
        iter: RustExpr,
        body: Vec<RustStmt>,
    },
    Loop {
        body: Vec<RustStmt>,
    },
    Break,
    Continue,
    Raw(String),
    Expr(RustExpr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustAssignTarget {
    Path(String),
    Field {
        base: RustExpr,
        field: String,
    },
    Tuple(Vec<RustAssignTarget>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustAssignOp {
    Assign,
    AddAssign,
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
    Borrow(Box<RustExpr>),
    Call {
        callee: Box<RustExpr>,
        args: Vec<RustExpr>,
    },
    MacroCall {
        path: Vec<String>,
        args: Vec<RustExpr>,
    },
    Field {
        base: Box<RustExpr>,
        field: String,
    },
    Index {
        base: Box<RustExpr>,
        index: Box<RustExpr>,
    },
    Match {
        scrutinee: Box<RustExpr>,
        arms: Vec<RustMatchArm>,
    },
    Unary {
        op: RustUnaryOp,
        expr: Box<RustExpr>,
    },
    Binary {
        op: RustBinaryOp,
        left: Box<RustExpr>,
        right: Box<RustExpr>,
    },
    Tuple(Vec<RustExpr>),
    StructLiteral {
        path: Vec<String>,
        fields: Vec<RustStructLiteralField>,
    },
    Closure {
        params: Vec<RustParam>,
        return_type: String,
        body: Vec<RustStmt>,
    },
    Range {
        start: Option<Box<RustExpr>>,
        end: Option<Box<RustExpr>>,
        inclusive: bool,
    },
    Try(Box<RustExpr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustStructLiteralField {
    pub name: String,
    pub value: RustExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustUnaryOp {
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustBinaryOp {
    Add,
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
pub struct RustMatchArm {
    pub pattern: RustPattern,
    pub guard: Option<RustExpr>,
    pub value: RustExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustPattern {
    Wildcard,
    Binding(String),
    Int(i64),
    Bool(bool),
    String(String),
    Tuple(Vec<RustPattern>),
    Slice {
        prefix: Vec<RustPattern>,
        rest: Option<String>,
        suffix: Vec<RustPattern>,
    },
    Or(Vec<RustPattern>),
    BindingAt {
        name: String,
        pattern: Box<RustPattern>,
    },
    Struct {
        path: Vec<String>,
        fields: Vec<RustPatternField>,
    },
    Range {
        start: Option<i64>,
        end: Option<i64>,
        inclusive: bool,
    },
    Variant {
        path: Vec<String>,
        payload: Option<Box<RustPattern>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustPatternField {
    pub name: String,
    pub pattern: RustPattern,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustDestructurePattern {
    Name(String),
    Ignore,
    Tuple(Vec<RustDestructurePattern>),
}
