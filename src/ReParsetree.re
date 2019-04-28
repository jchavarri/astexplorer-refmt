open Migrate_parsetree.Ast_404.Parsetree;

let rec handlePattern = ({ppat_desc, ppat_loc, ppat_attributes}) => [%js
  {
    val type_ = "pattern" |> Js.string;
    val ppat_desc_ = handlePatternDesc(ppat_desc);
    val ppat_loc_ = ReLocation.handleLocation(ppat_loc);
    val ppat_attributes_ =
      List.map(handleAttribute, ppat_attributes) |> Array.of_list |> Js.array
  }
]
and handlePatternDesc = patternDesc => {
  switch (patternDesc) {
  | Ppat_any =>
    %js
    {val type_ = "Ppat_any" |> Js.string}
  | Ppat_var(stringLoc) =>
    [%js
      {
        val type_ = "Ppat_var" |> Js.string;
        val string_loc_ = ReAsttypes.stringLoc(stringLoc)
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_alias(pattern, stringLoc) =>
    [%js
      {
        val type_ = "Ppat_alias" |> Js.string;
        val pattern = handlePattern(pattern);
        val string_loc_ = ReAsttypes.stringLoc(stringLoc)
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_constant(constant) =>
    [%js
      {
        val type_ = "Ppat_constant" |> Js.string;
        val constant = handleConstant(constant)
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_interval(constant1, constant2) =>
    [%js
      {
        val type_ = "Ppat_interval" |> Js.string;
        val constant1 = handleConstant(constant1);
        val constant2 = handleConstant(constant2)
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_tuple(patterns) =>
    [%js
      {
        val type_ = "Ppat_tuple" |> Js.string;
        val patterns =
          patterns |> List.map(handlePattern) |> Array.of_list |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_construct(_idLoc, _maybePattern) =>
    %js
    {val type_ = "TODO: Ppat_construct" |> Js.string}
  | Ppat_variant(_label, _maybePattern) =>
    %js
    {val type_ = "TODO: Ppat_variant" |> Js.string}
  | Ppat_record(_idLocsPatterns, _closedFlag) =>
    %js
    {val type_ = "TODO: Ppat_record" |> Js.string}
  | Ppat_array(_patterns) =>
    %js
    {val type_ = "TODO: Ppat_array" |> Js.string}
  | Ppat_or(_patternA, _patternB) =>
    %js
    {val type_ = "TODO: Ppat_or" |> Js.string}
  | Ppat_constraint(_pattern, _coreType) =>
    %js
    {val type_ = "TODO: Ppat_constraint" |> Js.string}
  | Ppat_type(_idLoc) =>
    %js
    {val type_ = "TODO: Ppat_type" |> Js.string}
  | Ppat_lazy(_pattern) =>
    %js
    {val type_ = "TODO: Ppat_lazy" |> Js.string}
  | Ppat_unpack(_stringLoc) =>
    %js
    {val type_ = "TODO: Ppat_unpack" |> Js.string}
  | Ppat_exception(_pattern) =>
    %js
    {val type_ = "TODO: Ppat_exception" |> Js.string}
  | Ppat_extension(_extension) =>
    %js
    {val type_ = "TODO: Ppat_extension" |> Js.string}
  | Ppat_open(_idLoc, _pattern) =>
    %js
    {val type_ = "TODO: Ppat_open" |> Js.string}
  };
}
and handleExpressionDesc = exprDesc =>
  switch (exprDesc) {
  | Pexp_ident(_idLoc) =>
    %js
    {val type_ = "TODO: Pexp_ident" |> Js.string}
  | Pexp_constant(constant) =>
    [%js
      {
        val type_ = "Pexp_constant" |> Js.string;
        val constant = handleConstant(constant)
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_let(_recFlag, _valueBindings, _expression) =>
    %js
    {val type_ = "TODO: Pexp_let" |> Js.string}
  | Pexp_function(_cases) =>
    %js
    {val type_ = "TODO: Pexp_function" |> Js.string}
  | Pexp_fun(argLabel, maybeExpression, pattern, expression) =>
    [%js
      {
        val type_ = "Pexp_fun" |> Js.string;
        val arg_label_ = ReAsttypes.handleArgLabel(argLabel);
        val arg_expression_ =
          maybeExpression |> Utils.handleOption(handleExpression);
        val pattern = handlePattern(pattern);
        val expression = handleExpression(expression)
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_apply(_expression, _argLabelExpressions) =>
    %js
    {val type_ = "TODO: Pexp_apply" |> Js.string}
  | Pexp_match(_expression, _cases) =>
    %js
    {val type_ = "TODO: Pexp_match" |> Js.string}
  | Pexp_try(_expression, _cases) =>
    %js
    {val type_ = "TODO: Pexp_try" |> Js.string}
  | Pexp_tuple(_expressions) =>
    %js
    {val type_ = "TODO: Pexp_tuple" |> Js.string}
  | Pexp_construct(_idLoc, _maybeExpression) =>
    %js
    {val type_ = "TODO: Pexp_construct" |> Js.string}
  | Pexp_variant(_label, _maybeExpression) =>
    %js
    {val type_ = "TODO: Pexp_variant" |> Js.string}
  | Pexp_record(_idLocExpressions, _maybeExpression) =>
    %js
    {val type_ = "TODO: Pexp_record" |> Js.string}
  | Pexp_field(_expression, _idLoc) =>
    %js
    {val type_ = "TODO: Pexp_field" |> Js.string}
  | Pexp_setfield(_expression, _idLoc, _fieldExpression) =>
    %js
    {val type_ = "TODO: Pexp_setfield" |> Js.string}
  | Pexp_array(_expressions) =>
    %js
    {val type_ = "TODO: Pexp_array" |> Js.string}
  | Pexp_ifthenelse(_ifExpression, _thenExpression, _maybeExpression) =>
    %js
    {val type_ = "TODO: Pexp_ifthenelse" |> Js.string}
  | Pexp_sequence(_expression, _seqExpression) =>
    %js
    {val type_ = "TODO: Pexp_sequence" |> Js.string}
  | Pexp_while(_condExpression, _expression) =>
    %js
    {val type_ = "TODO: Pexp_while" |> Js.string}
  | Pexp_for(
      _pattern,
      _startExpression,
      _endExpression,
      _directionFlag,
      _expression,
    ) =>
    %js
    {val type_ = "TODO: Pexp_for" |> Js.string}
  | Pexp_constraint(_expression, _core_type) =>
    %js
    {val type_ = "TODO: Pexp_constraint" |> Js.string}
  | Pexp_coerce(_expression, _maybeCoreType, _core_type) =>
    %js
    {val type_ = "TODO: Pexp_coerce" |> Js.string}
  | Pexp_send(_expression, _labelLoc) =>
    %js
    {val type_ = "TODO: Pexp_send" |> Js.string}
  | Pexp_new(_idLoc) =>
    %js
    {val type_ = "TODO: Pexp_new" |> Js.string}
  | Pexp_setinstvar(_labelLoc, _expression) =>
    %js
    {val type_ = "TODO: Pexp_setinstvar" |> Js.string}
  | Pexp_override(_labelLocExpressions) =>
    %js
    {val type_ = "TODO: Pexp_override" |> Js.string}
  | Pexp_letmodule(_stringLoc, _module_expr, _expression) =>
    %js
    {val type_ = "TODO: Pexp_letmodule" |> Js.string}
  | Pexp_letexception(_extension_constructor, _expression) =>
    %js
    {val type_ = "TODO: Pexp_letexception" |> Js.string}
  | Pexp_assert(_expression) =>
    %js
    {val type_ = "TODO: Pexp_assert" |> Js.string}
  | Pexp_lazy(_expression) =>
    %js
    {val type_ = "TODO: Pexp_lazy" |> Js.string}
  | Pexp_poly(_expression, _maybeCoreType) =>
    %js
    {val type_ = "TODO: Pexp_poly" |> Js.string}
  | Pexp_object(_class_structure) =>
    %js
    {val type_ = "TODO: Pexp_object" |> Js.string}
  | Pexp_newtype(_stringLoc, _expression) =>
    %js
    {val type_ = "TODO: Pexp_newtype" |> Js.string}
  | Pexp_pack(_module_expr) =>
    %js
    {val type_ = "TODO: Pexp_pack" |> Js.string}
  | Pexp_open(_overrideFlag, _idLoc, _expression) =>
    %js
    {val type_ = "TODO: Pexp_open" |> Js.string}
  | Pexp_extension(_extension) =>
    %js
    {val type_ = "TODO: Pexp_extension" |> Js.string}
  | Pexp_unreachable =>
    %js
    {val type_ = "TODO: Pexp_unreachable" |> Js.string}
  }
and handleExpression = ({pexp_desc, pexp_loc, pexp_attributes}) => [%js
  {
    val type_ = "expression" |> Js.string;
    val pexp_desc_ = handleExpressionDesc(pexp_desc);
    val pexp_loc_ = ReLocation.handleLocation(pexp_loc);
    val pexp_attributes_ =
      List.map(handleAttribute, pexp_attributes) |> Array.of_list |> Js.array
  }
]
and handleAttribute = ((loc, payload)) =>
  Utils.unsafeFromTuple((ReAsttypes.stringLoc(loc), handlePayload(payload)))
and handlePayload = payload =>
  switch (payload) {
  | PStr(_structure) => "PStr" |> Js.string
  | PSig(_signature) => "PSig" |> Js.string
  | PTyp(_core_type) => "PTyp" |> Js.string
  | PPat(_pattern, _maybeExpression) => "PPat" |> Js.string
  }
and handleConstant = constant =>
  switch (constant) {
  | Pconst_integer(string, suffix) =>
    %js
    {
      val type_ = "Pconst_integer" |> Js.string;
      val string = string |> Js.string;
      val suffix =
        suffix |> Utils.handleOption(c => c |> String.make(1) |> Js.string)
    }
  | Pconst_char(char) =>
    [%js
      {
        val type_ = "Pconst_char" |> Js.string;
        val char = char |> String.make(1) |> Js.string
      }
    ]
    |> Js.Unsafe.coerce
  | Pconst_string(string, quotationDelimiter) =>
    [%js
      {
        val type_ = "Pconst_string" |> Js.string;
        val string = string |> Js.string;
        val quotation_delimiter_ =
          quotationDelimiter |> Utils.handleOption(Js.string)
      }
    ]
    |> Js.Unsafe.coerce
  | Pconst_float(string, suffix) =>
    [%js
      {
        val type_ = "Pconst_float" |> Js.string;
        val string = string |> Js.string;
        val suffix =
          suffix |> Utils.handleOption(c => c |> String.make(1) |> Js.string)
      }
    ]
    |> Js.Unsafe.coerce
  };

let handleValueBinding = ({pvb_pat, pvb_expr, pvb_attributes, pvb_loc}) => [%js
  {
    val type_ = "value_binding" |> Js.string;
    val pvb_pat_ = handlePattern(pvb_pat);
    val pvb_expr_ = handleExpression(pvb_expr);
    val pvb_attributes_ =
      List.map(handleAttribute, pvb_attributes) |> Array.of_list |> Js.array;
    val pvb_loc_ = ReLocation.handleLocation(pvb_loc)
  }
];

let handleStrItemDesc = strItemDesc =>
  switch (strItemDesc) {
  | Pstr_eval(_expression, _attributes) =>
    %js
    {val type_ = "TODO: Pstr_eval" |> Js.string}
  | Pstr_value(recFlag, valueBindings) =>
    [%js
      {
        val type_ = "Pstr_value" |> Js.string;
        val rec_flag_ = recFlag |> ReAsttypes.recFlag;
        val value_bindings_ =
          valueBindings
          |> List.map(handleValueBinding)
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Pstr_primitive(_valueDescription) =>
    %js
    {val type_ = "TODO: Pstr_primitive" |> Js.string}
  | Pstr_type(_recFlag, _typeDeclarations) =>
    %js
    {val type_ = "TODO: Pstr_type" |> Js.string}
  | Pstr_typext(_typeExtension) =>
    %js
    {val type_ = "TODO: Pstr_typext" |> Js.string}
  | Pstr_exception(_extensionConstructor) =>
    %js
    {val type_ = "TODO: Pstr_exception" |> Js.string}
  | Pstr_module(_moduleBindings) =>
    %js
    {val type_ = "TODO: Pstr_module" |> Js.string}
  | Pstr_recmodule(_moduleBindings) =>
    %js
    {val type_ = "TODO: Pstr_recmodule" |> Js.string}
  | Pstr_modtype(_module_type_declaration) =>
    %js
    {val type_ = "TODO: Pstr_modtype" |> Js.string}
  | Pstr_open(_open_description) =>
    %js
    {val type_ = "TODO: Pstr_open" |> Js.string}
  | Pstr_class(_classDeclarations) =>
    %js
    {val type_ = "TODO: Pstr_class" |> Js.string}
  | Pstr_class_type(_classTypeDeclarations) =>
    %js
    {val type_ = "TODO: Pstr_class_type" |> Js.string}
  | Pstr_include(_include_declaration) =>
    %js
    {val type_ = "TODO: Pstr_include" |> Js.string}
  | Pstr_attribute(_attribute) =>
    %js
    {val type_ = "TODO: Pstr_attribute" |> Js.string}
  | Pstr_extension(_extension, _attributes) =>
    %js
    {val type_ = "TODO: Pstr_extension" |> Js.string}
  };

let handleStructureItem = ({pstr_desc, pstr_loc}) => {
  %js
  {
    val type_ = "structure_item" |> Js.string;
    val pstr_desc_ = handleStrItemDesc(pstr_desc);
    val pstr_loc_ = ReLocation.handleLocation(pstr_loc)
  };
};

let handleStructure = structure => {
  List.map(handleStructureItem, structure) |> Array.of_list |> Js.array;
};
