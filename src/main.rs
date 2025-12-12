use macros::cf;

fn main() {
    println!(
        "{}",
        cf!(

          decl -> importdecl | assdecl.
          impdecl -> "import" ident ("," ident)* "from" ident | "import" ident.
          assdecl -> ident (params)? "=" expr.
          params -> param | param "," params.
          actions -> action "for" item "in" iterable.
          range -> openR | closedR | halfL | halfR.
          openR -> "[" ord "," ord "]" | ord le ord le ord.
          closedR -> "(" ord "," ord ")" | ord lt ord lt ord.
          halfL -> "[" ord "," ord ")" | ord le ord lt ord.
          halfR -> "(" ord "," ord "]" | ord lt ord le ord.
          digit -> 0..9.
          op -> ass | eq | lt | gt | le | ge | and | or | not.
          ass -> "=".
          eq -> "==".
          lt -> "<".
          gt -> ">".
          le -> "<=".
          ge -> ">=".
          and -> "&&".
          or -> "||".
        )
    );
}
