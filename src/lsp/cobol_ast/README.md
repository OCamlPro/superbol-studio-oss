# Cobol_ast package

This package contains the abstract versions of the AST, to be specialized in other packages.

For API documentation, please see [index.mld].

## Toplevel module

* cobol_ast.ml

## AST Definition

* ast.ml: includes

  * simple_statements.ml
  * branching_statements.ml
  * data_descr.ml
  * misc_descr.ml
  * terms.ml
    * numericals.ml
  * operands.ml

* raw.ml : defines divisions over AST

## Helpers and Visitors

* traveral.ml
* helpers.ml
* raw_visitor.ml: main visitor entry point
  * raw_compilation_group_visitor.ml: visitor on compilation groups
  * raw_data_division_visitor.ml
  * raw_data_sections_visitor.ml
  * raw_misc_sections_visitor.ml
  * raw_proc_division_visitor.ml
  * raw_statements_visitor.ml
* operands_visitor.ml
* terms_visitor.ml

* abstract.ml: module types with abstract types
* abstract_visitor.ml: classes of visitors with abstract types
