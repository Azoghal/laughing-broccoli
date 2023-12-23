# laughing-broccoli
Practice with LALRPOP and Inkwell

## Parser
First pass complete, probably a few things will pop up as codegen developed.

**Todo**
- Do something like graphviz for bassoon to visualise the AST
- Refactor test setup so that more dependent on structure, less dependent on string formatting

---

## Codegen
**Todo**
- Read back through bassoon codegen to get the gist.
- Proof of concept code emission with Inkwell
- Codegen for existing language features
    - starting with just main function
    - only int type
    - assign to registers
    - binops for those its
- Codegen for new language features
    - arrays
    - indexing

Readme bump to trigger CI