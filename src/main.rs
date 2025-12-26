use macros::cf;

fn main() {
    println!(
        "{}",
        cf!(
          decls -> decl | decl decls.
          decl -> importdecl | assdecl.
          importdecl -> "import" ident ("," ident)* "from" ident | "import" ident.
          assdecl -> ident (params)? "=" expr | cmpndassdecl.
          cmpndassdecl -> ident cmpndass expr. // ?
          params -> param | param "," params.
          expr -> infixr | infixl | comp | assexpr.
          comp -> expr "for" item "in" iterable.
          assexpr -> expr cmpndass expr.
          range -> openR | closedR | halfL | halfR.
          openR -> "[" ord "," ord "]" | ord le ord le ord.
          closedR -> "(" ord "," ord ")" | ord lt ord lt ord.
          halfL -> "[" ord "," ord ")" | ord le ord lt ord.
          halfR -> "(" ord "," ord "]" | ord lt ord le ord.
          ident -> ( alpha | underscore ) (alpha | digit | underscore)* .
          digit -> '0'..'9'.
          alpha -> "A"..="Z" | "a"..= "z".
          underscore -> "_".
          op -> ass
            | cmpndass
            | plus
            | min
            | mul
            | div
            | mod
            | eq
            | ne
            | lt
            | gt
            | le
            | ge
            | and
            | or
            | not
            | opt.
          ass -> "=".
          cmpndass -> pluseq | mineq | muleq | diveq.
          pluseq -> "+=".
          mineq -> "-=".
          muleq -> "*=".
          diveq -> "/=".
          plus -> "+".
          min -> "-".
          mul -> "*".
          div -> "/".
          opt -> "?".
          eq -> "==".
          ne -> "!=".
          lt -> "<".
          gt -> ">".
          le -> "<=".
          ge -> ">=".
          or -> "||".
          and -> "&&".

        )
    );
}
