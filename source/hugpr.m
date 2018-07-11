BeginPackage["hugpr`", {"Notation`"}]

PrettyPrint::usage = "Prints the given variable and its Value in a new cell"
SetPrettyPrint::usage = "Prints the given variable and its then assigned value in a new cell"

MySave::usage = "Bla bla missing description"

Gauß::usage = "Calculates the propagated error of uncorrelated normal errors using the gauß formula. Usage: Gauß[ expression,  {{Symbol, Variance}, ...}, {Symbol->Mean, ...}]. Options are PrintTableOverview and PrintRowOverview." 
Größtfehler::usage = "Bla"

StatError::usage = "Assumes something about t-factor and such, and should be upgraded"

Begin["`Private`"]



(* Define two new Operator, that "log" the assignemt of the variables in a nicer way. *)
(* [ESC]=p[ESC] acts like =, but also prints an output cell with   "[LHS] = [RHS]"    *)
(* [ESC]=?[ESC] is used after an variable, to print like the above,  but without set  *)

PrettyPrintColor = RGBColor[0.0, 0.0, 0.5] (*Used in PrettyPrint, SetPrettyPrint*)

Clear[PrettyPrint];
PrettyPrint[x_Symbol] := 
	CellPrint[
		Style[TextCell[
			Row[{
				ExpressionCell[HoldForm[x]],
				" = ",
				ExpressionCell[x]}]
			], 
   			PrettyPrintColor
   		]
   	];
SetAttributes[PrettyPrint, HoldAll];
Notation[
	ParsedBoxWrapper[
		RowBox[{
			"x_",
			" ",
			TagBox[
				SuperscriptBox["=", "?"],
				PrettyPrintOperator,
				Editable -> False, Selectable -> False
       		]
       	}]
	]
	\[DoubleLongRightArrow] 
    ParsedBoxWrapper[RowBox[{"PrettyPrint", "[", "x_", "]"}]]
];
AddInputAlias[
	"=?" -> ParsedBoxWrapper[
		TagBox[SuperscriptBox["=", "?"],
		PrettyPrintOperator,
		Editable -> False, Selectable -> False]
	]
];


Clear[SetPrettyPrint];
SetPrettyPrint[x_, y_] := 
	Module[{},
		CellPrint[
			Style[
				TextCell[Row[{
					ExpressionCell[HoldForm[x]],
					" = ", 
       	 			ExpressionCell[y]
       	 		}]],
      	 		PrettyPrintColor
			]
   		];
   		x = y;
	];  	
SetAttributes[SetPrettyPrint, HoldAll];
Notation[
	ParsedBoxWrapper[RowBox[{"x_", " ", TagBox[OverscriptBox["=", Cell["PRINT"]], SetPrettyPrintOperator, Editable -> False, Selectable -> False], " ", "y_"}]]
	\[DoubleLongLeftRightArrow] 
	ParsedBoxWrapper[
		RowBox[{"SetPrettyPrint", "[", RowBox[{"x_", ",", " ", "y_"}], "]"}]
	]
];
AddInputAlias[
	"=p" -> ParsedBoxWrapper[
		TagBox[OverscriptBox["=", Cell["PRINT"]],
		SetPrettyPrintOperator,
		Editable -> False, Selectable -> False]
	]
];


Clear[MySave]
Options[MySave] = {"FileEnding" -> "png", "DoBackup" -> True, "BackupPath" -> "backup"}
MySave[g_, filename_String, OptionsPattern[]] :=
Block[
{
	backupdirectory,
	backupfilepath,
	localdirectory,
	result
},
	localdirectory = NotebookDirectory[];
	
	If[OptionValue["DoBackup"],
		backupdirectory = StringJoin[{localdirectory, OptionValue["BackupPath"], "\\"}];
		If[ \[Not] DirectoryQ[backupdirectory], CreateDirectory[backupdirectory]];
		If[ \[Not] DirectoryQ[backupdirectory], Print["Could not create Backupdir!"]];
 		
		backupfilepath = StringJoin[{backupdirectory, filename, "-",
   			DateString[{"Year", "-", "Month", "-",  "Day", "-", "Hour", "-", 
   			"Minute", "-", "Second"}],
   			".",
   			OptionValue["FileEnding"]}
   		];
		
		Print@StringJoin["Backuppath: ", backupfilepath];
		result = Export[backupfilepath, g];
		If[result === $Failed, Print@"Export return $Failed, backup-path correct?", Print@StringJoin["saved to ", ToString@result]]
	];
	
	result = Export[StringJoin [{localdirectory, filename, ".", OptionValue["FileEnding"]}], g];
	If[result === $Failed, Print@"Export returned $Failed, path correct?", Print@StringJoin["saved to ", ToString@result]]; 
	"Saved!"
];


(* Propagate uncorrelated gauß errors and return the resulting error
	Arg1: Expression to be propagated
	Arg2: List of {symbol, normal error}
	Arg3: List of mean values, {symbol->mean, symbol2->mean}
	
	Options:
	PrintRowOverview -> Boolean: Prints the derivatives, the square of their evaluated value,
		the given error squared, and the final summand under the square root
	PrintTableOverview -> Boolean: Like RowOverview, but in a table*)
Clear[Gauß];
Options[Gauß] = {"PrintRowOverview" -> False, "PrintTableOverview" -> False};
Gauß[formel_, fehler_, werte_, OptionsPattern[]] :=
Module[{sum, u,results, helfer, gridOutput},
	gridOutput = If[OptionValue["PrintTableOverview"],
			{{
				"\!\(x\)",
				"\!\(\*SubscriptBox[\(\[PartialD]\), \(x\)]\)f",
				"(\!\(\*SubscriptBox[\(\[PartialD]\), \(x\)]\)f\!\(\*SubscriptBox[\(|\), \(X\)]\)\!\(\*SuperscriptBox[\()\), \(2\)]\)", 
				"\!\(\*SubsuperscriptBox[\(u\), \(x\), \(2\)]\)",
				"(\!\(\*SubscriptBox[\(\[PartialD]\), \(x\)]\)f\!\(\*SubscriptBox[\(|\), \(x\)]\)\!\(\*SuperscriptBox[\()\), \(2\)]\)*\!\(\*SubsuperscriptBox[\(u\), \(x\), \(2\)]\)"
			}},
			{}
		];

	helfer[fe_] :=
	Module[{a, b, c},
		a = D[formel, fe[[1]]];
		b = (a^2) /. werte;
		c = b * fe[[2]]^2;
		If[OptionValue["PrintRowOverview"],
			Print[
				StringForm["\!\(\*SubscriptBox[\(\[PartialD]\), \(`1`\)]\) f = `2` \n", TraditionalForm@fe[[1]], TraditionalForm@a], (* Partielle Ableitung *)
				StringForm["(\!\(\*SubscriptBox[\(\[PartialD]\), \(`1`\)]\) f\!\(\*SuperscriptBox[\()\), \(2\)]\)\!\(\*SubscriptBox[\(|\), SubscriptBox[\(x\), \(i\)]]\) = `2` \[TildeTilde] `3` \n", TraditionalForm@fe[[1]], TraditionalForm@b, N@b], (* Partielle Ableitung ausgewertet*)
				StringForm["\!\(\*SubsuperscriptBox[\(u\), \(`1`\), \(2\)]\) = `2` \[TildeTilde] `3` \n", TraditionalForm@fe[[1]], TraditionalForm@(fe[[2]]^2), N@fe[[2]]^2], (* Unsicherheitsquadrat *)
				StringForm["(\!\(\*SubscriptBox[\(\[PartialD]\), \(`1`\)]\) \!\(\*SuperscriptBox[\(f)\), \(2\)]\)\!\(\*SubscriptBox[\(|\), \(X\)]\)*\!\(\*SubsuperscriptBox[\(u\), \(`1`\), \(2\)]\) = `2` \[TildeTilde] `3` \n", TraditionalForm@fehler[[1]], TraditionalForm@c, N@c] (* Endwert für die Summation *)
			];     
		];
		If[OptionValue["PrintTableOverview"],
			AppendTo[gridOutput, {TraditionalForm@fe[[1]], TraditionalForm@a, NumberForm[b, 4], NumberForm[fe[[2]]^2, 4], NumberForm[c, 4]}]
		];
		c
	];	
	
	results = Map[(helfer[#]&), fehler];
	sum = Total[results];
	u = Sqrt[sum];
	If[OptionValue["PrintRowOverview"],
		Print[StringForm["\!\(\(u\) = `1` \[TildeTilde] `2`\)", formel/.werte, N[formel/.werte]]];
		Print[StringForm["\!\(\*SubscriptBox[\(u\), \(f\)]\) = `1` \[TildeTilde] `2`", u, N@u]]
    ];
    If[OptionValue["PrintTableOverview"], Print[Grid[gridOutput, Frame->All, Spacings->{1, 1}]]];  
    u  
];


Clear[Größtfehler];
Options[Größtfehler] = {"PrintRowOverview" -> False, "PrintTableOverview" -> False};
Größtfehler[formel_, fehler_, werte_, OptionsPattern[]] :=
Module[{sum, u,results, helfer, gridOutput},
	gridOutput = If[OptionValue["PrintTableOverview"],
			{{
				"\!\(x\)",
				"\!\(\*SubscriptBox[\(\[PartialD]\), \(x\)]\)f",
				"\!\(\*SubscriptBox[\(\[PartialD]\), \(x\)]\)f\!\(\*SubscriptBox[\(|\), \(X\)]\)", 
				"\!\(\*SubscriptBox[\(u\), \(x\)]\)",
				"\!\(\*SubscriptBox[\(\[PartialD]\), \(x\)]\)f\!\(\*SubscriptBox[\(|\), \(x\)]\)*\!\(\*SubscriptBox[\(u\), \(x\)]\)"
			}},
			{}
		];

	helfer[fe_] :=
	Module[{a, b, c},
		a = D[formel, fe[[1]]];
		b = Abs[a /. werte];
		c = Abs[b * fe[[2]]];
		If[OptionValue["PrintRowOverview"],
			Print[
				StringForm["\!\(\*SubscriptBox[\(\[PartialD]\), \(`1`\)]\) f = `2` \n", TraditionalForm@fe[[1]], TraditionalForm@a], (* Partielle Ableitung *)
				StringForm["\!\(\*SubscriptBox[\(\[PartialD]\), \(`1`\)]\) f \!\(\*SubscriptBox[\(|\), \(X\)]\) = `2` \[TildeTilde] `3` \n", TraditionalForm@fe[[1]], TraditionalForm@b, N@b], (* Partielle Ableitung ausgewertet*)
				StringForm["\!\(\*SubscriptBox[\(u\), \(`1`\)]\) = `2` \[TildeTilde] `3` \n", TraditionalForm@fe[[1]], TraditionalForm@Abs@(fe[[2]]), Abs@N@fe[[2]]], (* Unsicherheitsbetrag *)
				StringForm["\!\(\*SubscriptBox[\(\[PartialD]\), \(`1`\)]\) f\!\(\*SubscriptBox[\(|\), \(X\)]\)*\!\(\*SubscriptBox[\(u\), \(`1`\)]\) = `2` \[TildeTilde] `3` \n", TraditionalForm@fehler[[1]], TraditionalForm@c, N@c] (* Endwert für die Summation *)
			];     
		];
		If[OptionValue["PrintTableOverview"],
			AppendTo[gridOutput, {TraditionalForm@fe[[1]], TraditionalForm@a, NumberForm[b, 4], NumberForm[fe[[2]], 4], NumberForm[c, 4]}]
		];
		c
	];	
	
	results = Map[(helfer[#]&), fehler];
	sum = Total[results];
	u = sum;
	If[OptionValue["PrintRowOverview"],
		Print[StringForm["\!\(\(u\) = `1` \[TildeTilde] `2`\)", formel/.werte, N[formel/.werte]]];
		Print[StringForm["\!\(\*SubscriptBox[\(u\), \(f\)]\) = `1` \[TildeTilde] `2`", u, N@u]]
    ];
    If[OptionValue["PrintTableOverview"], Print[Grid[gridOutput, Frame->All, Spacings->{1, 1}]]];  
    u  
];




StatError[list_List] := 
Module[{n}, 
	n = Length[list];
	StandardDeviation[list]/(n*n-n)^0.5
]


End[];
EndPackage[];