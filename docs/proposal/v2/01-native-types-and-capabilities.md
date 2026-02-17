# Native Types and Capability Contracts

## Problem

Elevate currently inherits too much behavior from Rust type/method metadata. This couples semantic decisions to rustdex availability and Rust method surfaces that are not always meaningful in Elevate.

## Direction

Define Elevate-native types and capability contracts as the source of truth. Rust lowering becomes a translation step, not semantic discovery.

## Native Type Model (Core)

```text
Scalars:   Int8..128, UInt8..128, Float32..64, Bool
Text:      String
Collections: List<T>, Map<K, V>, Set<T>
Algebraic: Option<T>, Result<T, E>, Tuple<...>
Nominal:   StructId, EnumId, NewtypeId
Function:  Fn<(T1..Tn), R>
Iter:      Iter<T>

// Deliberately Not Including
usize, isize - equivalents, the compiler will cast intelligently

// Some useful derived types
Byte = UInt8
ByteString = List<Byte>
```

Notes:
- Concrete syntax can still use familiar names, but semantic identity is Elevate-owned.
- No assumption that `Int` equals Rust `i64` at semantic layer.

## Capability Contracts

Each type family exposes explicit capabilities instead of implicit Rust methods.

Example contract shape:

```text
Capability: list.push
Receiver: mut List<T>
Args: [T]
Returns: Unit
Effects: [mutate::receiver]
```

```text
Capability: map.get
Receiver: read Map<K, V>
Args: [K]
Returns: Option<V>
Effects: [read::receiver]
```

## Resolution Pipeline

```mermaid
flowchart TD
    A[Typed expression: recv.op(args)] --> B[Resolve receiver Elevate type]
    B --> C[Lookup op in Elevate capability registry]
    C --> D{Found?}
    D -- no --> E[Emit Elevate capability diagnostic]
    D -- yes --> F[Apply arg/receiver/effect checks]
    F --> G[Emit typed op node in Core EIR]
```

## Interop Boundary

- No automatic method passthrough to Rust core/std methods.
- Foreign behavior only via declared adapters.
- Adapter signatures must declare borrow/ownership behavior explicitly.

## rustdex Impact

What can be removed from core path:

1. Method existence checks via rustdex.
2. Trait method signature inference via rustdex.
3. rustdex preflight as compile prerequisite.
4. Borrow trait implementation queries via rustdex.

What may remain temporarily optional:

1. Tooling to bootstrap adapter signatures from rustdoc metadata.
2. Migration linting for old Rust-coupled callsites.

## Trait Policy (Recommended)

- Elevate core: constrain or remove trait-driven method resolution.
- Keep abstraction through generics + capability rows + explicit interfaces.
- Rust trait requirements for host frameworks move behind adapter wrappers.