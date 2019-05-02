open Migrate_parsetree.Ast_404.Parsetree;

let rec handleModuleType = ({pmty_desc, pmty_loc, pmty_attributes}) => [%js
  {
    val type_ = "module_type" |> Js.string;
    val pmty_desc_ = handleModuleTypeDesc(pmty_desc);
    val pmty_loc_ = ReLocation.handleLocation(pmty_loc);
    val pmty_attributes_ =
      pmty_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array
  }
]
and handleModuleTypeDesc = desc =>
  switch (desc) {
  | Pmty_ident(idLoc) =>
    [%js
      {
        val type_ = "Pmty_ident" |> Js.string;
        val id_loc_ = idLoc |> ReAsttypes.handleIdLoc
      }
    ]
    |> Js.Unsafe.coerce
  | Pmty_signature(signature) =>
    [%js
      {
        val type_ = "Pmty_signature" |> Js.string;
        val signature =
          signature
          |> List.map(handleSignatureItem)
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Pmty_functor(stringLoc, inputModType, outputModType) =>
    [%js
      {
        val type_ = "Pmty_functor" |> Js.string;
        val string_loc_ = stringLoc |> ReAsttypes.handleStringLoc;
        val input_mod_type_ =
          inputModType |> Utils.handleOption(handleModuleType);
        val output_mod_type_ = outputModType |> handleModuleType
      }
    ]
    |> Js.Unsafe.coerce
  | Pmty_with(moduleType, withContraint) =>
    [%js
      {
        val type_ = "Pmty_with" |> Js.string;
        val module_type_ = moduleType |> handleModuleType;
        val with_constraint_ =
          withContraint
          |> List.map(handleWithConstraint)
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Pmty_typeof(moduleExpr) =>
    [%js
      {
        val type_ = "Pmty_typeof" |> Js.string;
        val module_expr_ = moduleExpr |> handleModuleExpr
      }
    ]
    |> Js.Unsafe.coerce
  | Pmty_extension(extension) =>
    [%js
      {
        val type_ = "Pmty_extension" |> Js.string;
        val extension = extension |> handleExtension
      }
    ]
    |> Js.Unsafe.coerce
  | Pmty_alias(idLoc) =>
    %js
    {
      val type_ = "Pmty_alias" |> Js.string;
      val id_loc_ = idLoc |> ReAsttypes.handleIdLoc
    }
  }
and handleWithConstraint = withConstraint =>
  switch (withConstraint) {
  | Pwith_type(idLoc, typeDeclaration) =>
    [%js
      {
        val type_ = "Pwith_type" |> Js.string;
        val id_loc_ = idLoc |> ReAsttypes.handleIdLoc;
        val type_declaration_ = typeDeclaration |> handleTypeDeclaration
      }
    ]
    |> Js.Unsafe.coerce
  | Pwith_module(idLoc1, idLoc2) =>
    [%js
      {
        val type_ = "Pwith_module" |> Js.string;
        val id_loc1_ = idLoc1 |> ReAsttypes.handleIdLoc;
        val id_loc2_ = idLoc2 |> ReAsttypes.handleIdLoc
      }
    ]
    |> Js.Unsafe.coerce
  | Pwith_typesubst(typeDeclaration) =>
    [%js
      {
        val type_ = "Pwith_typesubst" |> Js.string;
        val type_declaration_ = typeDeclaration |> handleTypeDeclaration
      }
    ]
    |> Js.Unsafe.coerce
  | Pwith_modsubst(idLoc1, idLoc2) =>
    %js
    {
      val type_ = "Pwith_modsubst" |> Js.string;
      val id_loc1_ = idLoc1 |> ReAsttypes.handleStringLoc;
      val id_loc2_ = idLoc2 |> ReAsttypes.handleIdLoc
    }
  }
and handleSignatureItem = ({psig_desc, psig_loc}) => [%js
  {
    val type_ = "signature_item" |> Js.string;
    val psig_desc_ = handleSignatureItemDesc(psig_desc);
    val psig_loc_ = ReLocation.handleLocation(psig_loc)
  }
]
and handleSignatureItemDesc = desc =>
  switch (desc) {
  | Psig_value(valueDescription) =>
    [%js
      {
        val type_ = "Psig_value" |> Js.string;
        val value_description_ = valueDescription |> handleValueDescription
      }
    ]
    |> Js.Unsafe.coerce
  | Psig_type(recFlag, typeDeclarations) =>
    [%js
      {
        val type_ = "Psig_type" |> Js.string;
        val rec_flag_ = recFlag |> ReAsttypes.handleRecFlag;
        val type_declarations_ =
          typeDeclarations
          |> List.map(handleTypeDeclaration)
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Psig_typext(typeExtension) =>
    [%js
      {
        val type_ = "Psig_typext" |> Js.string;
        val type_extension_ = typeExtension |> handleTypeExtension
      }
    ]
    |> Js.Unsafe.coerce
  | Psig_exception(extensionConstructor) =>
    [%js
      {
        val type_ = "Psig_exception" |> Js.string;
        val extension_constructor_ =
          extensionConstructor |> handleExtensionConstructor
      }
    ]
    |> Js.Unsafe.coerce
  | Psig_module(moduleDeclaration) =>
    [%js
      {
        val type_ = "Psig_module" |> Js.string;
        val module_declaration_ = moduleDeclaration |> handleModuleDeclaration
      }
    ]
    |> Js.Unsafe.coerce
  | Psig_recmodule(moduleDeclarations) =>
    [%js
      {
        val type_ = "Psig_recmodule" |> Js.string;
        val module_declarations_ =
          moduleDeclarations
          |> List.map(handleModuleDeclaration)
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Psig_modtype(moduleTypeDeclaration) =>
    [%js
      {
        val type_ = "Psig_modtype" |> Js.string;
        val module_type_declaration_ =
          moduleTypeDeclaration |> handleModuleTypeDeclaration
      }
    ]
    |> Js.Unsafe.coerce
  | Psig_open(openDescription) =>
    [%js
      {
        val type_ = "Psig_open" |> Js.string;
        val open_description_ = openDescription |> handleOpenDescription
      }
    ]
    |> Js.Unsafe.coerce
  | Psig_include(includeDescription) =>
    [%js
      {
        val type_ = "Psig_include" |> Js.string;
        val include_description_ =
          includeDescription |> handleIncludeDescription
      }
    ]
    |> Js.Unsafe.coerce
  | Psig_class(classDescriptions) =>
    [%js
      {
        val type_ = "Psig_class" |> Js.string;
        val class_descriptions_ =
          classDescriptions
          |> List.map(handleClassDescription)
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Psig_class_type(classTypeDeclarations) =>
    [%js
      {
        val type_ = "Psig_class_type" |> Js.string;
        val class_type_declarations_ =
          classTypeDeclarations
          |> List.map(handleClassTypeDeclaration)
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Psig_attribute(attribute) =>
    [%js
      {
        val type_ = "Psig_attribute" |> Js.string;
        val attribute = attribute |> handleAttribute
      }
    ]
    |> Js.Unsafe.coerce
  | Psig_extension(extension, attributes) =>
    [%js
      {
        val type_ = "Psig_extension" |> Js.string;
        val extension = extension |> handleExtension;
        val attributes =
          attributes |> List.map(handleAttribute) |> Array.of_list |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  }
and handleModuleDeclaration = ({pmd_name, pmd_type, pmd_attributes, pmd_loc}) => [%js
  {
    val type_ = "module_declaration" |> Js.string;
    val pmd_name_ = pmd_name |> ReAsttypes.handleStringLoc;
    val pmd_type_ = pmd_type |> handleModuleType;
    val pmd_attributes_ =
      pmd_attributes |> List.map(handleAttribute) |> Array.of_list |> Js.array;
    val pmd_loc_ = pmd_loc |> ReLocation.handleLocation
  }
]
and handleModuleTypeDeclaration =
    ({pmtd_name, pmtd_type, pmtd_attributes, pmtd_loc}) => [%js
  {
    val type_ = "module_type_declaration" |> Js.string;
    val pmtd_name_ = pmtd_name |> ReAsttypes.handleStringLoc;
    val pmtd_type_ = pmtd_type |> Utils.handleOption(handleModuleType);
    val pmtd_attributes_ =
      pmtd_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array;
    val pmtd_loc_ = pmtd_loc |> ReLocation.handleLocation
  }
]
and handleOpenDescription =
    ({popen_lid, popen_override, popen_loc, popen_attributes}) => [%js
  {
    val type_ = "open_description" |> Js.string;
    val popen_lid_ = popen_lid |> ReAsttypes.handleIdLoc;
    val popen_override_ = popen_override |> ReAsttypes.handleOverrideFlag;
    val popen_loc_ = popen_loc |> ReLocation.handleLocation;
    val popen_attributes_ =
      popen_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array
  }
]
and handleIncludeDescription = ({pincl_mod, pincl_loc, pincl_attributes}) => [%js
  {
    val type_ = "include_description" |> Js.string;
    val pincl_mod_ = pincl_mod |> handleModuleType;
    val pincl_loc_ = pincl_loc |> ReLocation.handleLocation;
    val pincl_attributes_ =
      pincl_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array
  }
]
and handleIncludeDeclaration = ({pincl_mod, pincl_loc, pincl_attributes}) => [%js
  {
    val type_ = "include_declaration" |> Js.string;
    val pincl_mod_ = pincl_mod |> handleModuleExpr;
    val pincl_loc_ = pincl_loc |> ReLocation.handleLocation;
    val pincl_attributes_ =
      pincl_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array
  }
]
and handleModuleExpr = ({pmod_desc, pmod_loc, pmod_attributes}) => [%js
  {
    val type_ = "module_expr" |> Js.string;
    val pmod_desc_ = handleModuleExprDesc(pmod_desc);
    val pmod_loc_ = ReLocation.handleLocation(pmod_loc);
    val pmod_attributes_ =
      pmod_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array
  }
]
and handleModuleExprDesc = moduleExpr =>
  switch (moduleExpr) {
  | Pmod_ident(idLoc) =>
    [%js
      {
        val type_ = "Pmod_ident" |> Js.string;
        val id_loc_ = ReAsttypes.handleIdLoc(idLoc)
      }
    ]
    |> Js.Unsafe.coerce
  | Pmod_structure(structure) =>
    [%js
      {
        val type_ = "Pmod_structure" |> Js.string;
        val structure = structure |> handleStructure
      }
    ]
    |> Js.Unsafe.coerce
  | Pmod_functor(stringLoc, maybeModuleType, moduleExpr) =>
    [%js
      {
        val type_ = "Pmod_functor" |> Js.string;
        val string_loc_ = stringLoc |> ReAsttypes.handleStringLoc;
        val module_type_ =
          maybeModuleType |> Utils.handleOption(handleModuleType);
        val module_expr_ = moduleExpr |> handleModuleExpr
      }
    ]
    |> Js.Unsafe.coerce
  | Pmod_apply(moduleExpr1, moduleExpr2) =>
    [%js
      {
        val type_ = "Pmod_apply" |> Js.string;
        val module_expr1_ = moduleExpr1 |> handleModuleExpr;
        val module_expr2_ = moduleExpr2 |> handleModuleExpr
      }
    ]
    |> Js.Unsafe.coerce
  | Pmod_constraint(moduleExpr, moduleType) =>
    [%js
      {
        val type_ = "Pmod_constraint" |> Js.string;
        val module_expr_ = moduleExpr |> handleModuleExpr;
        val module_type_ = moduleType |> handleModuleType
      }
    ]
    |> Js.Unsafe.coerce
  | Pmod_unpack(expression) =>
    [%js
      {
        val type_ = "Pmod_unpack" |> Js.string;
        val expression = expression |> handleExpression
      }
    ]
    |> Js.Unsafe.coerce
  | Pmod_extension(extension) =>
    %js
    {
      val type_ = "Pmod_extension" |> Js.string;
      val extension = extension |> handleExtension
    }
  }
and handleCase = ({pc_lhs, pc_guard, pc_rhs}) => [%js
  {
    val type_ = "case" |> Js.string;
    val pc_lhs_ = pc_lhs |> handlePattern;
    val pc_guard_ = pc_guard |> Utils.handleOption(handleExpression);
    val pc_rhs_ = pc_rhs |> handleExpression
  }
]
and handleValueDescription =
    ({pval_name, pval_type, pval_prim, pval_attributes, pval_loc}) => [%js
  {
    val type_ = "value_description" |> Js.string;
    val pval_name_ = pval_name |> ReAsttypes.handleStringLoc;
    val pval_type_ = pval_type |> handleCoreType;
    val pval_prim_ =
      pval_prim |> List.map(Js.string) |> Array.of_list |> Js.array;
    val pval_attributes_ =
      pval_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array;
    val pval_loc_ = pval_loc |> ReLocation.handleLocation
  }
]
and handleTypeDeclaration =
    (
      {
        ptype_name,
        ptype_params,
        ptype_cstrs,
        ptype_kind,
        ptype_private,
        ptype_manifest,
        ptype_attributes,
        ptype_loc,
      },
    ) => [%js
  {
    val type_ = "type_declaration" |> Js.string;
    val ptype_name_ = ptype_name |> ReAsttypes.handleStringLoc;
    val ptype_params_ =
      ptype_params
      |> List.map(((coreType, variance)) =>
           (handleCoreType(coreType), ReAsttypes.handleVariance(variance))
           |> Utils.unsafeFromTuple
         )
      |> Array.of_list
      |> Js.array;
    val ptype_cstrs_ =
      ptype_cstrs
      |> List.map(((constraint1, constraint2, location)) =>
           (
             handleCoreType(constraint1),
             handleCoreType(constraint2),
             ReLocation.handleLocation(location),
           )
           |> Utils.unsafeFromTuple
         )
      |> Array.of_list
      |> Js.array;
    val ptype_kind_ = ptype_kind |> handleTypeKind;
    val ptype_private_ = ptype_private |> ReAsttypes.handlePrivateFlag;
    val ptype_manifest_ =
      ptype_manifest |> Utils.handleOption(handleCoreType);
    val ptype_attributes_ =
      ptype_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array;
    val ptype_loc_ = ptype_loc |> ReLocation.handleLocation
  }
]
and handleTypeKind = kind =>
  switch (kind) {
  | Ptype_abstract =>
    [%js {val type_ = "Ptype_abstract" |> Js.string}] |> Js.Unsafe.coerce
  | Ptype_variant(constructorDeclarations) =>
    [%js
      {
        val type_ = "Ptype_variant" |> Js.string;
        val constructor_declarations_ =
          constructorDeclarations
          |> List.map(handleConstructorDeclaration)
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Ptype_record(labelDeclarations) =>
    [%js
      {
        val type_ = "Ptype_record" |> Js.string;
        val label_declarations_ =
          labelDeclarations
          |> List.map(handleLabelDeclaration)
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Ptype_open =>
    %js
    {val type_ = "Ptype_open" |> Js.string}
  }
and handlePattern = ({ppat_desc, ppat_loc, ppat_attributes}) => [%js
  {
    val type_ = "pattern" |> Js.string;
    val ppat_desc_ = handlePatternDesc(ppat_desc);
    val ppat_loc_ = ReLocation.handleLocation(ppat_loc);
    val ppat_attributes_ =
      ppat_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array
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
        val string_loc_ = ReAsttypes.handleStringLoc(stringLoc)
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_alias(pattern, stringLoc) =>
    [%js
      {
        val type_ = "Ppat_alias" |> Js.string;
        val pattern = handlePattern(pattern);
        val string_loc_ = ReAsttypes.handleStringLoc(stringLoc)
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
  | Ppat_construct(idLoc, maybePattern) =>
    [%js
      {
        val type_ = "Ppat_construct" |> Js.string;
        val id_loc_ = ReAsttypes.handleIdLoc(idLoc);
        val pattern = maybePattern |> Utils.handleOption(handlePattern)
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_variant(label, maybePattern) =>
    [%js
      {
        val type_ = "Ppat_variant" |> Js.string;
        val label = label |> Js.string;
        val pattern = maybePattern |> Utils.handleOption(handlePattern)
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_record(idLocsPatterns, closedFlag) =>
    [%js
      {
        val type_ = "Ppat_record" |> Js.string;
        val patterns =
          idLocsPatterns
          |> List.map(((idLoc, pattern)) =>
               (ReAsttypes.handleIdLoc(idLoc), handlePattern(pattern))
               |> Utils.unsafeFromTuple
             )
          |> Array.of_list
          |> Js.array;
        val closed_flag_ = ReAsttypes.handleClosedFlag(closedFlag)
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_array(patterns) =>
    [%js
      {
        val type_ = "Ppat_array" |> Js.string;
        val patterns =
          patterns |> List.map(handlePattern) |> Array.of_list |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_or(patternA, patternB) =>
    [%js
      {
        val type_ = "Ppat_or" |> Js.string;
        val patternA = patternA |> handlePattern;
        val patternB = patternB |> handlePattern
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_constraint(pattern, coreType) =>
    [%js
      {
        val type_ = "Ppat_constraint" |> Js.string;
        val pattern = pattern |> handlePattern;
        val core_type_ = coreType |> handleCoreType
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_type(idLoc) =>
    [%js
      {
        val type_ = "Ppat_type" |> Js.string;
        val id_loc_ = ReAsttypes.handleIdLoc(idLoc)
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_lazy(pattern) =>
    [%js
      {
        val type_ = "Ppat_lazy" |> Js.string;
        val pattern = pattern |> handlePattern
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_unpack(stringLoc) =>
    [%js
      {
        val type_ = "Ppat_unpack" |> Js.string;
        val string_loc_ = ReAsttypes.handleStringLoc(stringLoc)
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_exception(pattern) =>
    [%js
      {
        val type_ = "Ppat_exception" |> Js.string;
        val pattern = pattern |> handlePattern
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_extension(extension) =>
    [%js
      {
        val type_ = "Ppat_extension" |> Js.string;
        val extension = handleExtension(extension)
      }
    ]
    |> Js.Unsafe.coerce
  | Ppat_open(idLoc, pattern) =>
    [%js
      {
        val type_ = "Ppat_open" |> Js.string;
        val id_loc_ = ReAsttypes.handleIdLoc(idLoc);
        val pattern = pattern |> handlePattern
      }
    ]
    |> Js.Unsafe.coerce
  };
}
and handleExpressionDesc = exprDesc =>
  switch (exprDesc) {
  | Pexp_ident(idLoc) =>
    [%js
      {
        val type_ = "Pexp_ident" |> Js.string;
        val id_loc_ = ReAsttypes.handleIdLoc(idLoc)
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_constant(constant) =>
    [%js
      {
        val type_ = "Pexp_constant" |> Js.string;
        val constant = handleConstant(constant)
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_let(recFlag, valueBindings, expression) =>
    [%js
      {
        val type_ = "Pexp_let" |> Js.string;
        val rec_flag_ = recFlag |> ReAsttypes.handleRecFlag;
        val value_bindings_ =
          valueBindings
          |> List.map(handleValueBinding)
          |> Array.of_list
          |> Js.array;
        val expression = handleExpression(expression)
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_function(cases) =>
    [%js
      {
        val type_ = "Pexp_function" |> Js.string;
        val cases = cases |> List.map(handleCase) |> Array.of_list |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_fun(argLabel, argExpression, pattern, expression) =>
    [%js
      {
        val type_ = "Pexp_fun" |> Js.string;
        val arg_label_ = ReAsttypes.handleArgLabel(argLabel);
        val arg_expression_ =
          argExpression |> Utils.handleOption(handleExpression);
        val pattern = handlePattern(pattern);
        val expression = handleExpression(expression)
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_apply(expression, args) =>
    [%js
      {
        val type_ = "Pexp_apply" |> Js.string;
        val expression = handleExpression(expression);
        val args =
          args
          |> List.map(((label, argExpr)) =>
               (ReAsttypes.handleArgLabel(label), handleExpression(argExpr))
               |> Utils.unsafeFromTuple
             )
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_match(expression, cases) =>
    [%js
      {
        val type_ = "Pexp_match" |> Js.string;
        val expression = expression |> handleExpression;
        val cases = cases |> List.map(handleCase) |> Array.of_list |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_try(expression, cases) =>
    [%js
      {
        val type_ = "Pexp_try" |> Js.string;
        val expression = expression |> handleExpression;
        val cases = cases |> List.map(handleCase) |> Array.of_list |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_tuple(expressions) =>
    [%js
      {
        val type_ = "Pexp_tuple" |> Js.string;
        val expressions =
          expressions
          |> List.map(handleExpression)
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_construct(idLoc, maybeExpression) =>
    [%js
      {
        val type_ = "Pexp_construct" |> Js.string;
        val id_loc_ = ReAsttypes.handleIdLoc(idLoc);
        val expression =
          maybeExpression |> Utils.handleOption(handleExpression)
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_variant(label, maybeExpression) =>
    [%js
      {
        val type_ = "Pexp_variant" |> Js.string;
        val label = label |> Js.string;
        val expression =
          maybeExpression |> Utils.handleOption(handleExpression)
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_record(idLocExpressions, maybeExpression) =>
    [%js
      {
        val type_ = "Pexp_construct" |> Js.string;
        val fields =
          idLocExpressions
          |> List.map(((idLoc, expression)) =>
               (ReAsttypes.handleIdLoc(idLoc), handleExpression(expression))
               |> Utils.unsafeFromTuple
             )
          |> Array.of_list
          |> Js.array;
        val expression =
          maybeExpression |> Utils.handleOption(handleExpression)
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_field(expression, idLoc) =>
    [%js
      {
        val type_ = "Pexp_field" |> Js.string;
        val expression = expression |> handleExpression;
        val id_loc_ = ReAsttypes.handleIdLoc(idLoc)
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_setfield(expression, idLoc, fieldExpression) =>
    [%js
      {
        val type_ = "Pexp_setfield" |> Js.string;
        val expression = expression |> handleExpression;
        val id_loc_ = ReAsttypes.handleIdLoc(idLoc);
        val field_expr_ = fieldExpression |> handleExpression
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_array(expressions) =>
    [%js
      {
        val type_ = "Pexp_array" |> Js.string;
        val expressions =
          expressions
          |> List.map(handleExpression)
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_ifthenelse(ifExpression, thenExpression, elseExpression) =>
    [%js
      {
        val type_ = "Pexp_ifthenelse" |> Js.string;
        val if_expr_ = ifExpression |> handleExpression;
        val then_expr_ = thenExpression |> handleExpression;
        val else_expr_ =
          elseExpression |> Utils.handleOption(handleExpression)
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_sequence(expression, seqExpression) =>
    [%js
      {
        val type_ = "Pexp_sequence" |> Js.string;
        val expression = expression |> handleExpression;
        val seq_expr_ = seqExpression |> handleExpression
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_while(condExpression, expression) =>
    [%js
      {
        val type_ = "Pexp_while" |> Js.string;
        val cond_expr = condExpression |> handleExpression;
        val expression = expression |> handleExpression
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_for(
      pattern,
      startExpression,
      endExpression,
      directionFlag,
      expression,
    ) =>
    [%js
      {
        val type_ = "Pexp_for" |> Js.string;
        val pattern = pattern |> handlePattern;
        val start_expr_ = startExpression |> handleExpression;
        val end_expr_ = endExpression |> handleExpression;
        val direction_flag_ = directionFlag |> ReAsttypes.handleDirectionFlag;
        val expression = expression |> handleExpression
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_constraint(expression, coreType) =>
    [%js
      {
        val type_ = "Pexp_constraint" |> Js.string;
        val expression = expression |> handleExpression;
        val core_type_ = coreType |> handleCoreType
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_coerce(expression, maybeCoreType, coreType) =>
    [%js
      {
        val type_ = "Pexp_coerce" |> Js.string;
        val expression = expression |> handleExpression;
        val opt_core_type_ =
          maybeCoreType |> Utils.handleOption(handleCoreType);
        val core_type_ = coreType |> handleCoreType
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_send(expression, label) =>
    [%js
      {
        val type_ = "Pexp_send" |> Js.string;
        val expression = expression |> handleExpression;
        val label = label |> Js.string
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_new(idLoc) =>
    [%js
      {
        val type_ = "Pexp_new" |> Js.string;
        val id_loc_ = ReAsttypes.handleIdLoc(idLoc)
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_setinstvar(labelLoc, expression) =>
    [%js
      {
        val type_ = "Pexp_setinstvar" |> Js.string;
        val label_loc_ = ReAsttypes.handleStringLoc(labelLoc);
        val expression = expression |> handleExpression
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_override(labelLocExpressions) =>
    [%js
      {
        val type_ = "Pexp_override" |> Js.string;
        val expressions =
          labelLocExpressions
          |> List.map(((stringLoc, expression)) =>
               (
                 ReAsttypes.handleStringLoc(stringLoc),
                 handleExpression(expression),
               )
               |> Utils.unsafeFromTuple
             )
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_letmodule(stringLoc, moduleExpr, expression) =>
    [%js
      {
        val type_ = "Pexp_letmodule" |> Js.string;
        val string_loc_ = ReAsttypes.handleStringLoc(stringLoc);
        val module_expr_ = handleModuleExpr(moduleExpr);
        val expression = expression |> handleExpression
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_letexception(extensionConstructor, expression) =>
    [%js
      {
        val type_ = "Pexp_letexception" |> Js.string;
        val extension_constructor_ =
          extensionConstructor |> handleExtensionConstructor;
        val expression = expression |> handleExpression
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_assert(expression) =>
    [%js
      {
        val type_ = "Pexp_assert" |> Js.string;
        val expression = expression |> handleExpression
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_lazy(expression) =>
    [%js
      {
        val type_ = "Pexp_lazy" |> Js.string;
        val expression = expression |> handleExpression
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_poly(expression, maybeCoreType) =>
    [%js
      {
        val type_ = "Pexp_poly" |> Js.string;
        val expression = expression |> handleExpression;
        val core_type_ = maybeCoreType |> Utils.handleOption(handleCoreType)
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_object(classStructure) =>
    [%js
      {
        val type_ = "Pexp_object" |> Js.string;
        val class_structure_ = classStructure |> handleClassStructure
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_newtype(label, expression) =>
    [%js
      {
        val type_ = "Pexp_newtype" |> Js.string;
        val label = label |> Js.string;
        val expression = expression |> handleExpression
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_pack(moduleExpr) =>
    [%js
      {
        val type_ = "Pexp_pack" |> Js.string;
        val module_expr_ = handleModuleExpr(moduleExpr)
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_open(overrideFlag, idLoc, expression) =>
    [%js
      {
        val type_ = "Pexp_open" |> Js.string;
        val override_flag_ = overrideFlag |> ReAsttypes.handleOverrideFlag;
        val id_loc_ = ReAsttypes.handleIdLoc(idLoc);
        val expression = expression |> handleExpression
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_extension(extension) =>
    [%js
      {
        val type_ = "Pexp_extension" |> Js.string;
        val extension = extension |> handleExtension
      }
    ]
    |> Js.Unsafe.coerce
  | Pexp_unreachable =>
    %js
    {val type_ = "Pexp_unreachable" |> Js.string}
  }
and handleClassType = ({pcty_desc, pcty_loc, pcty_attributes}) => [%js
  {
    val type_ = "class_type" |> Js.string;
    val pcty_desc_ = pcty_desc |> handleClassTypeDesc;
    val pcty_loc_ = pcty_loc |> ReLocation.handleLocation;
    val pcty_attributes_ =
      pcty_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array
  }
]
and handleClassTypeDesc = classTypeDesc =>
  switch (classTypeDesc) {
  | Pcty_constr(idLoc, coreTypes) =>
    [%js
      {
        val type_ = "Pcty_constr" |> Js.string;
        val id_loc_ = idLoc |> ReAsttypes.handleIdLoc;
        val core_types_ =
          coreTypes |> List.map(handleCoreType) |> Array.of_list |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Pcty_signature(classSignature) =>
    [%js
      {
        val type_ = "Pcty_signature" |> Js.string;
        val class_signature = classSignature |> handleClassSignature
      }
    ]
    |> Js.Unsafe.coerce
  | Pcty_arrow(argLabel, coreType, classType) =>
    [%js
      {
        val type_ = "Pcty_arrow" |> Js.string;
        val arg_label_ = ReAsttypes.handleArgLabel(argLabel);
        val core_type_ = coreType |> handleCoreType;
        val class_type_ = classType |> handleClassType
      }
    ]
    |> Js.Unsafe.coerce
  | Pcty_extension(extension) =>
    %js
    {
      val type_ = "Pcty_extension" |> Js.string;
      val extension = extension |> handleExtension
    }
  }
and handleClassSignature = ({pcsig_self, pcsig_fields}) => [%js
  {
    val type_ = "class_signature" |> Js.string;
    val pcsig_self_ = pcsig_self |> handleCoreType;
    val pcsig_fields_ =
      pcsig_fields
      |> List.map(handleClassTypeField)
      |> Array.of_list
      |> Js.array
  }
]
and handleClassTypeField = ({pctf_desc, pctf_loc, pctf_attributes}) => [%js
  {
    val type_ = "class_type_field" |> Js.string;
    val pctf_desc_ = pctf_desc |> handleClassTypeFieldDesc;
    val pctf_loc_ = pctf_loc |> ReLocation.handleLocation;
    val pctf_attributes_ =
      pctf_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array
  }
]
and handleClassTypeFieldDesc = classTypeFieldDesc =>
  switch (classTypeFieldDesc) {
  | Pctf_inherit(classType) =>
    [%js
      {
        val type_ = "Pctf_inherit" |> Js.string;
        val class_type_ = classType |> handleClassType
      }
    ]
    |> Js.Unsafe.coerce
  | Pctf_val((labelLoc, mutableFlag, virtualFlag, coreType)) =>
    [%js
      {
        val type_ = "Pctf_val" |> Js.string;
        val label_loc_ = labelLoc |> Js.string;
        val mutable_flag_ = mutableFlag |> ReAsttypes.handleMutableFlag;
        val virtual_flag_ = virtualFlag |> ReAsttypes.handleVirtualFlag;
        val core_type_ = coreType |> handleCoreType
      }
    ]
    |> Js.Unsafe.coerce
  | Pctf_method((labelLoc, privateFlag, virtualFlag, coreType)) =>
    [%js
      {
        val type_ = "Pctf_method" |> Js.string;
        val label_loc_ = labelLoc |> Js.string;
        val private_flag_ = privateFlag |> ReAsttypes.handlePrivateFlag;
        val virtual_flag_ = virtualFlag |> ReAsttypes.handleVirtualFlag;
        val core_type_ = coreType |> handleCoreType
      }
    ]
    |> Js.Unsafe.coerce
  | Pctf_constraint((coreType1, coreType2)) =>
    [%js
      {
        val type_ = "Pctf_constraint" |> Js.string;
        val core_type_1_ = coreType1 |> handleCoreType;
        val core_type_2_ = coreType2 |> handleCoreType
      }
    ]
    |> Js.Unsafe.coerce
  | Pctf_attribute(attribute) =>
    [%js
      {
        val type_ = "Pctf_attribute" |> Js.string;
        val attribute = attribute |> handleAttribute
      }
    ]
    |> Js.Unsafe.coerce
  | Pctf_extension(extension) =>
    %js
    {
      val type_ = "Pctf_extension" |> Js.string;
      val extension = extension |> handleExtension
    }
  }
and handleClassInfosClassType =
    (
      type_,
      {pci_virt, pci_params, pci_name, pci_expr, pci_loc, pci_attributes},
    ) => [%js
  {
    val type_ = type_ |> Js.string;
    val pci_virt_ = pci_virt |> ReAsttypes.handleVirtualFlag;
    val pci_params_ =
      pci_params
      |> List.map(((coreType, variance)) =>
           (handleCoreType(coreType), ReAsttypes.handleVariance(variance))
           |> Utils.unsafeFromTuple
         )
      |> Array.of_list
      |> Js.array;
    val pci_name_ = pci_name |> ReAsttypes.handleStringLoc;
    val pci_expr_ = pci_expr |> handleClassType;
    val pci_loc_ = pci_loc |> ReLocation.handleLocation;
    val pci_attributes_ =
      pci_attributes |> List.map(handleAttribute) |> Array.of_list |> Js.array
  }
]
and handleClassDescription = classDesc =>
  handleClassInfosClassType("class_description", classDesc)
and handleClassTypeDeclaration = classDesc =>
  handleClassInfosClassType("class_type_declaration", classDesc)
and handleClassStructure = ({pcstr_self, pcstr_fields}) => [%js
  {
    val type_ = "class_structure" |> Js.string;
    val pcstr_self_ = handlePattern(pcstr_self);
    val pcstr_fields_ =
      pcstr_fields |> List.map(handleClassField) |> Array.of_list |> Js.array
  }
]
and handleClassExpr = ({pcl_desc, pcl_loc, pcl_attributes}) => [%js
  {
    val type_ = "class_expr" |> Js.string;
    val pcl_desc_ = pcl_desc |> handleClassExprDesc;
    val pcl_loc_ = pcl_loc |> ReLocation.handleLocation;
    val pcl_attributes_ =
      pcl_attributes |> List.map(handleAttribute) |> Array.of_list |> Js.array
  }
]
and handleClassExprDesc = classExprDesc =>
  switch (classExprDesc) {
  | Pcl_constr(idLoc, coreTypes) =>
    [%js
      {
        val type_ = "Pcl_constr" |> Js.string;
        val id_loc_ = idLoc |> ReAsttypes.handleIdLoc;
        val core_types_ =
          coreTypes |> List.map(handleCoreType) |> Array.of_list |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Pcl_structure(classStructure) =>
    [%js
      {
        val type_ = "Pcl_structure" |> Js.string;
        val class_structure_ = classStructure |> handleClassStructure
      }
    ]
    |> Js.Unsafe.coerce
  | Pcl_fun(argLabel, argExpression, pattern, classExpr) =>
    [%js
      {
        val type_ = "Pcl_fun" |> Js.string;
        val arg_label_ = argLabel |> ReAsttypes.handleArgLabel;
        val arg_expression_ =
          argExpression |> Utils.handleOption(handleExpression);
        val pattern = handlePattern(pattern);
        val class_expr_ = classExpr |> handleClassExpr
      }
    ]
    |> Js.Unsafe.coerce
  | Pcl_apply(classExpr, args) =>
    [%js
      {
        val type_ = "Pcl_apply" |> Js.string;
        val class_expr_ = classExpr |> handleClassExpr;
        val args =
          args
          |> List.map(((label, argExpr)) =>
               (ReAsttypes.handleArgLabel(label), handleExpression(argExpr))
               |> Utils.unsafeFromTuple
             )
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Pcl_let(recFlag, valueBindings, classExpr) =>
    [%js
      {
        val type_ = "Pcl_let" |> Js.string;
        val rec_flag_ = recFlag |> ReAsttypes.handleRecFlag;
        val value_bindings_ =
          valueBindings
          |> List.map(handleValueBinding)
          |> Array.of_list
          |> Js.array;
        val class_expr_ = classExpr |> handleClassExpr
      }
    ]
    |> Js.Unsafe.coerce
  | Pcl_constraint(classExpr, classType) =>
    [%js
      {
        val type_ = "Pcl_constraint" |> Js.string;
        val class_expr_ = classExpr |> handleClassExpr;
        val class_type_ = classType |> handleClassType
      }
    ]
    |> Js.Unsafe.coerce
  | Pcl_extension(extension) =>
    %js
    {
      val type_ = "Pcl_extension" |> Js.string;
      val extension = extension |> handleExtension
    }
  }
and handleClassField = ({pcf_desc, pcf_loc, pcf_attributes}) => [%js
  {
    val type_ = "class_field" |> Js.string;
    val pcf_desc_ = pcf_desc |> handleClassFieldDesc;
    val pcf_loc_ = ReLocation.handleLocation(pcf_loc);
    val pcf_attributes_ =
      pcf_attributes |> List.map(handleAttribute) |> Array.of_list |> Js.array
  }
]
and handleClassFieldDesc = cfd =>
  switch (cfd) {
  | Pcf_inherit(overrideFlag, classExpr, maybeStringLoc) =>
    [%js
      {
        val type_ = "Pcf_inherit" |> Js.string;
        val override_flag_ = overrideFlag |> ReAsttypes.handleOverrideFlag;
        val class_expr_ = classExpr |> handleClassExpr;
        val string_loc_ = maybeStringLoc |> Utils.handleOption(Js.string)
      }
    ]
    |> Js.Unsafe.coerce
  | Pcf_val((labelLoc, mutableFlag, classFieldKind)) =>
    [%js
      {
        val type_ = "Pcf_val" |> Js.string;
        val label_loc_ = labelLoc |> ReAsttypes.handleStringLoc;
        val mutable_flag_ = mutableFlag |> ReAsttypes.handleMutableFlag;
        val class_field_kind_ = classFieldKind |> handleClassFieldKind
      }
    ]
    |> Js.Unsafe.coerce
  | Pcf_method((labelLoc, privateFlag, classFieldKind)) =>
    [%js
      {
        val type_ = "Pcf_method" |> Js.string;
        val label_loc_ = labelLoc |> ReAsttypes.handleStringLoc;
        val private_flag_ = privateFlag |> ReAsttypes.handlePrivateFlag;
        val class_field_kind_ = classFieldKind |> handleClassFieldKind
      }
    ]
    |> Js.Unsafe.coerce
  | Pcf_constraint((coreType1, coreType2)) =>
    [%js
      {
        val type_ = "Pcf_constraint" |> Js.string;
        val core_type_1_ = coreType1 |> handleCoreType;
        val core_type_2_ = coreType2 |> handleCoreType
      }
    ]
    |> Js.Unsafe.coerce
  | Pcf_initializer(expression) =>
    [%js
      {
        val type_ = "Pcf_initializer" |> Js.string;
        val expression = expression |> handleExpression
      }
    ]
    |> Js.Unsafe.coerce
  | Pcf_attribute(attribute) =>
    [%js
      {
        val type_ = "Pcf_attribute" |> Js.string;
        val attribute = attribute |> handleAttribute
      }
    ]
    |> Js.Unsafe.coerce
  | Pcf_extension(extension) =>
    %js
    {
      val type_ = "Pcf_extension" |> Js.string;
      val extension = extension |> handleExtension
    }
  }
and handleClassFieldKind = cfk =>
  switch (cfk) {
  | Cfk_virtual(coreType) =>
    [%js
      {
        val type_ = "Cfk_virtual" |> Js.string;
        val core_type_ = coreType |> handleCoreType
      }
    ]
    |> Js.Unsafe.coerce
  | Cfk_concrete(overrideFlag, expression) =>
    [%js
      {
        val type_ = "Cfk_concrete" |> Js.string;
        val override_flag_ = overrideFlag |> ReAsttypes.handleOverrideFlag;
        val expression = expression |> handleExpression
      }
    ]
    |> Js.Unsafe.coerce
  }
and handleExtensionConstructor =
    ({pext_name, pext_kind, pext_loc, pext_attributes}) => [%js
  {
    val type_ = "extension_constructor" |> Js.string;
    val pext_name_ = ReAsttypes.handleStringLoc(pext_name);
    val pext_kind_ = pext_kind |> handleExtConstructorKind;
    val pexp_loc_ = ReLocation.handleLocation(pext_loc);
    val pext_attributes_ =
      pext_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array
  }
]
and handleExtConstructorKind = k =>
  switch (k) {
  | Pext_decl(constructorArguments, maybeCoreType) =>
    [%js
      {
        val type_ = "Pext_decl" |> Js.string;
        val constructor_arguments_ =
          constructorArguments |> handleConstructorArguments;
        val core_type_ = maybeCoreType |> Utils.handleOption(handleCoreType)
      }
    ]
    |> Js.Unsafe.coerce
  | Pext_rebind(idLoc) =>
    %js
    {
      val type_ = "Pext_rebind" |> Js.string;
      val id_loc_ = ReAsttypes.handleIdLoc(idLoc)
    }
  }
and handleTypeExtension =
    (
      {
        ptyext_path,
        ptyext_params,
        ptyext_constructors,
        ptyext_private,
        ptyext_attributes,
      },
    ) => [%js
  {
    val type_ = "type_extension" |> Js.string;
    val ptyext_path_ = ptyext_path |> ReAsttypes.handleIdLoc;
    val ptyext_params_ =
      ptyext_params
      |> List.map(((coreType, variance)) =>
           (handleCoreType(coreType), ReAsttypes.handleVariance(variance))
           |> Utils.unsafeFromTuple
         )
      |> Array.of_list
      |> Js.array;
    val ptyext_constructors_ =
      ptyext_constructors
      |> List.map(handleExtensionConstructor)
      |> Array.of_list
      |> Js.array;
    val ptyext_private_ = ptyext_private |> ReAsttypes.handlePrivateFlag;
    val ptyext_attributes_ =
      ptyext_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array
  }
]
and handleLabelDeclaration =
    ({pld_name, pld_mutable, pld_type, pld_loc, pld_attributes}) => [%js
  {
    val type_ = "label_declaration" |> Js.string;
    val pld_name_ = ReAsttypes.handleStringLoc(pld_name);
    val pld_mutable_ = pld_mutable |> ReAsttypes.handleMutableFlag;
    val pld_type_ = handleCoreType(pld_type);
    val pld_loc_ = ReLocation.handleLocation(pld_loc);
    val pld_attributes_ =
      pld_attributes |> List.map(handleAttribute) |> Array.of_list |> Js.array
  }
]
and handleConstructorDeclaration =
    ({pcd_name, pcd_args, pcd_res, pcd_loc, pcd_attributes}) => [%js
  {
    val type_ = "constructor_declaration" |> Js.string;
    val pcd_name_ = ReAsttypes.handleStringLoc(pcd_name);
    val pcd_args_ = pcd_args |> handleConstructorArguments;
    val pcd_res_ = pcd_res |> Utils.handleOption(handleCoreType);
    val pcd_loc_ = ReLocation.handleLocation(pcd_loc);
    val pcd_attributes_ =
      pcd_attributes |> List.map(handleAttribute) |> Array.of_list |> Js.array
  }
]
and handleConstructorArguments = c =>
  switch (c) {
  | Pcstr_tuple(coreTypes) =>
    [%js
      {
        val type_ = "Pcstr_tuple" |> Js.string;
        val core_types_ =
          coreTypes |> List.map(handleCoreType) |> Array.of_list |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Pcstr_record(labelDeclarations) =>
    %js
    {
      val type_ = "Pcstr_record" |> Js.string;
      val label_declarations_ =
        labelDeclarations
        |> List.map(handleLabelDeclaration)
        |> Array.of_list
        |> Js.array
    }
  }
and handleExpression = ({pexp_desc, pexp_loc, pexp_attributes}) => [%js
  {
    val type_ = "expression" |> Js.string;
    val pexp_desc_ = handleExpressionDesc(pexp_desc);
    val pexp_loc_ = ReLocation.handleLocation(pexp_loc);
    val pexp_attributes_ =
      pexp_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array
  }
]
and handleAttribute = ((loc, payload)) =>
  Utils.unsafeFromTuple((
    ReAsttypes.handleStringLoc(loc),
    handlePayload(payload),
  ))
and handleExtension = e => handleAttribute(e)
and handlePayload = payload =>
  switch (payload) {
  | PStr(structure) =>
    [%js
      {
        val type_ = "PStr" |> Js.string;
        val structure = structure |> handleStructure
      }
    ]
    |> Js.Unsafe.coerce
  | PSig(signature) =>
    [%js
      {
        val type_ = "PSig" |> Js.string;
        val signature =
          signature
          |> List.map(handleSignatureItem)
          |> Array.of_list
          |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | PTyp(coreType) =>
    [%js
      {
        val type_ = "PTyp" |> Js.string;
        val core_type_ = coreType |> handleCoreType
      }
    ]
    |> Js.Unsafe.coerce
  | PPat(pattern, maybeExpression) =>
    %js
    {
      val type_ = "PPat" |> Js.string;
      val pattern = pattern |> handlePattern;
      val expression = maybeExpression |> Utils.handleOption(handleExpression)
    }
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
  }
and handleValueBinding = ({pvb_pat, pvb_expr, pvb_attributes, pvb_loc}) => [%js
  {
    val type_ = "value_binding" |> Js.string;
    val pvb_pat_ = handlePattern(pvb_pat);
    val pvb_expr_ = handleExpression(pvb_expr);
    val pvb_attributes_ =
      pvb_attributes |> List.map(handleAttribute) |> Array.of_list |> Js.array;
    val pvb_loc_ = ReLocation.handleLocation(pvb_loc)
  }
]
and handleModuleBinding = ({pmb_name, pmb_expr, pmb_attributes, pmb_loc}) => [%js
  {
    val type_ = "module_binding" |> Js.string;
    val pmb_name_ = pmb_name |> ReAsttypes.handleStringLoc;
    val pmb_expr_ = pmb_expr |> handleModuleExpr;
    val pmb_attributes_ =
      pmb_attributes |> List.map(handleAttribute) |> Array.of_list |> Js.array;
    val pmb_loc_ = pmb_loc |> ReLocation.handleLocation
  }
]
and handleCoreType = ({ptyp_desc, ptyp_loc, ptyp_attributes}) => [%js
  {
    val type_ = "core_type" |> Js.string;
    val ptyp_desc_ = handleCoreTypeDesc(ptyp_desc);
    val ptyp_loc_ = ReLocation.handleLocation(ptyp_loc);
    val ptyp_attributes_ =
      ptyp_attributes
      |> List.map(handleAttribute)
      |> Array.of_list
      |> Js.array
  }
]
and handleCoreTypeDesc = coreTypeDesc =>
  switch (coreTypeDesc) {
  | Ptyp_any => [%js {val type_ = "Ptyp_any" |> Js.string}] |> Js.Unsafe.coerce
  | Ptyp_var(ident) =>
    [%js {val type_ = "Ptyp_var" |> Js.string; val ident = ident |> Js.string}]
    |> Js.Unsafe.coerce
  | Ptyp_arrow(argLabel, coreType1, coreType2) =>
    [%js
      {
        val type_ = "Ptyp_arrow" |> Js.string;
        val arg_label_ = argLabel |> ReAsttypes.handleArgLabel;
        val core_type_1_ = coreType1 |> handleCoreType;
        val core_type_2_ = coreType2 |> handleCoreType
      }
    ]
    |> Js.Unsafe.coerce
  | Ptyp_tuple(coreTypes) =>
    [%js
      {
        val type_ = "Ptyp_tuple" |> Js.string;
        val core_types_ =
          coreTypes |> List.map(handleCoreType) |> Array.of_list |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Ptyp_constr(idLoc, coreTypes) =>
    [%js
      {
        val type_ = "Ptyp_constr" |> Js.string;
        val id_loc_ = idLoc |> ReAsttypes.handleIdLoc;
        val core_types_ =
          coreTypes |> List.map(handleCoreType) |> Array.of_list |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Ptyp_object(objectFields, closedFlag) =>
    [%js
      {
        val type_ = "Ptyp_object" |> Js.string;
        val object_fields_ =
          objectFields
          |> List.map(((label, attributes, coreType)) =>
               (
                 label |> Js.string,
                 attributes
                 |> List.map(handleAttribute)
                 |> Array.of_list
                 |> Js.array,
                 handleCoreType(coreType),
               )
               |> Utils.unsafeFromTuple
             )
          |> Array.of_list
          |> Js.array;
        val closed_flag_ = closedFlag |> ReAsttypes.handleClosedFlag
      }
    ]
    |> Js.Unsafe.coerce
  | Ptyp_class(idLoc, coreTypes) =>
    [%js
      {
        val type_ = "Ptyp_class" |> Js.string;
        val id_loc_ = idLoc |> ReAsttypes.handleIdLoc;
        val core_types_ =
          coreTypes |> List.map(handleCoreType) |> Array.of_list |> Js.array
      }
    ]
    |> Js.Unsafe.coerce
  | Ptyp_alias(coreType, ident) =>
    [%js
      {
        val type_ = "Ptyp_alias" |> Js.string;
        val core_type_ = coreType |> handleCoreType;
        val ident = ident |> Js.string
      }
    ]
    |> Js.Unsafe.coerce
  | Ptyp_variant(rowFields, closedFlag, maybeLabels) =>
    [%js
      {
        val type_ = "Ptyp_variant" |> Js.string;
        val row_fields_ =
          rowFields |> List.map(handleRowField) |> Array.of_list |> Js.array;
        val closed_flag_ = ReAsttypes.handleClosedFlag(closedFlag);
        val labels =
          maybeLabels
          |> Utils.handleOption(labels =>
               labels |> List.map(Js.string) |> Array.of_list |> Js.array
             )
      }
    ]
    |> Js.Unsafe.coerce
  | Ptyp_poly(strLocs, coreType) =>
    [%js
      {
        val type_ = "Ptyp_poly" |> Js.string;
        val str_locs_ =
          strLocs |> List.map(Js.string) |> Array.of_list |> Js.array;
        val core_type_ = coreType |> handleCoreType
      }
    ]
    |> Js.Unsafe.coerce
  | Ptyp_package((idLoc, coreTypes)) =>
    [%js
      {
        val type_ = "Ptyp_package" |> Js.string;
        val package_type_ =
          (
            idLoc |> ReAsttypes.handleIdLoc,
            coreTypes
            |> List.map(((idLoc, coreType)) =>
                 (idLoc |> ReAsttypes.handleIdLoc, coreType |> handleCoreType)
                 |> Utils.unsafeFromTuple
               )
            |> Array.of_list
            |> Js.array,
          )
          |> Utils.unsafeFromTuple
      }
    ]
    |> Js.Unsafe.coerce
  | Ptyp_extension(extension) =>
    %js
    {
      val type_ = "Ptyp_extension" |> Js.string;
      val extension = extension |> handleExtension
    }
  }
and handleRowField = rowField =>
  switch (rowField) {
  | Rtag(_stringLoc, _attributes, _bool, _coreTypes) =>
    "TODO: Rtag" |> Js.string
  | Rinherit(_coreType) => "TODO: Rinherit" |> Js.string
  }
and handleStrItemDesc = strItemDesc =>
  switch (strItemDesc) {
  | Pstr_eval(_expression, _attributes) =>
    %js
    {val type_ = "TODO: Pstr_eval" |> Js.string}
  | Pstr_value(recFlag, valueBindings) =>
    [%js
      {
        val type_ = "Pstr_value" |> Js.string;
        val rec_flag_ = recFlag |> ReAsttypes.handleRecFlag;
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
  | Pstr_module(moduleBinding) =>
    [%js
      {
        val type_ = "Pstr_module" |> Js.string;
        val module_binding_ = moduleBinding |> handleModuleBinding
      }
    ]
    |> Js.Unsafe.coerce
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
  | Pstr_include(includeDeclaration) =>
    [%js
      {
        val type_ = "Pstr_include" |> Js.string;
        val include_declaration_ =
          includeDeclaration |> handleIncludeDeclaration
      }
    ]
    |> Js.Unsafe.coerce
  | Pstr_attribute(_attribute) =>
    %js
    {val type_ = "TODO: Pstr_attribute" |> Js.string}
  | Pstr_extension(_extension, _attributes) =>
    %js
    {val type_ = "TODO: Pstr_extension" |> Js.string}
  }
and handleStructureItem = ({pstr_desc, pstr_loc}) => {
  %js
  {
    val type_ = "structure_item" |> Js.string;
    val pstr_desc_ = handleStrItemDesc(pstr_desc);
    val pstr_loc_ = ReLocation.handleLocation(pstr_loc)
  };
}
and handleStructure = structure => {
  structure |> List.map(handleStructureItem) |> Array.of_list |> Js.array;
};
