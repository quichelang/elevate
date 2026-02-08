#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    RustUse(RustUse),
    RustBlock(String),
    Struct(StructDef),
    Enum(EnumDef),
    Trait(TraitDef),
    Impl(ImplBlock),
    Function(FunctionDef),
    Const(ConstDef),
    Static(StaticDef),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustUse {
    pub path: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDef {
    pub visibility: Visibility,
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef {
    pub visibility: Visibility,
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: String,
    pub payload: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitDef {
    pub visibility: Visibility,
    pub name: String,
    pub supertraits: Vec<Type>,
    pub methods: Vec<TraitMethodSig>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitMethodSig {
    pub name: String,
    pub type_params: Vec<GenericParam>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDef {
    pub visibility: Visibility,
    pub name: String,
    pub type_params: Vec<GenericParam>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericParam {
    pub name: String,
    pub bounds: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplBlock {
    pub target: String,
    pub methods: Vec<FunctionDef>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Const(ConstDef),
    DestructureConst {
        pattern: DestructurePattern,
        value: Expr,
        is_const: bool,
    },
    Assign {
        target: AssignTarget,
        op: AssignOp,
        value: Expr,
    },
    Return(Option<Expr>),
    If {
        condition: Expr,
        then_block: Block,
        else_block: Option<Block>,
    },
    While {
        condition: Expr,
        body: Block,
    },
    For {
        binding: DestructurePattern,
        iter: Expr,
        body: Block,
    },
    Loop {
        body: Block,
    },
    Break,
    Continue,
    RustBlock(String),
    Expr(Expr),
    TailExpr(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstDef {
    pub visibility: Visibility,
    pub name: String,
    pub ty: Option<Type>,
    pub value: Expr,
    pub is_const: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StaticDef {
    pub visibility: Visibility,
    pub name: String,
    pub ty: Type,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub path: Vec<String>,
    pub args: Vec<Type>,
    pub trait_bounds: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Char(char),
    String(String),
    Path(Vec<String>),
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    MacroCall {
        path: Vec<String>,
        args: Vec<Expr>,
    },
    Field {
        base: Box<Expr>,
        field: String,
    },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    StructLiteral {
        path: Vec<String>,
        fields: Vec<StructLiteralField>,
    },
    Closure {
        params: Vec<Param>,
        return_type: Option<Type>,
        body: Block,
    },
    Range {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        inclusive: bool,
    },
    Try(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructLiteralField {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignTarget {
    Path(String),
    Field {
        base: Box<Expr>,
        field: String,
    },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    Tuple(Vec<AssignTarget>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    Assign,
    AddAssign,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DestructurePattern {
    Name(String),
    Ignore,
    Tuple(Vec<DestructurePattern>),
    Slice {
        prefix: Vec<DestructurePattern>,
        rest: Option<String>,
        suffix: Vec<DestructurePattern>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
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
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Wildcard,
    Binding(String),
    Int(i64),
    Bool(bool),
    Char(char),
    String(String),
    Tuple(Vec<Pattern>),
    Slice {
        prefix: Vec<Pattern>,
        rest: Option<String>,
        suffix: Vec<Pattern>,
    },
    Or(Vec<Pattern>),
    BindingAt {
        name: String,
        pattern: Box<Pattern>,
    },
    Struct {
        path: Vec<String>,
        fields: Vec<PatternField>,
        has_rest: bool,
    },
    Range {
        start: Option<i64>,
        end: Option<i64>,
        inclusive: bool,
    },
    Variant {
        path: Vec<String>,
        payload: Option<Box<Pattern>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PatternField {
    pub name: String,
    pub pattern: Pattern,
}
