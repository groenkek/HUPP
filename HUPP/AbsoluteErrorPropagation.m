(* Wolfram Language Package *)

BeginPackage["HUPP`"]


Options[AbsoluteErrorPropagation] = {Analyze->False};
If[ !ValueQ[AbsoluteErrorPropagation::usage],
    AbsoluteErrorPropagation::usage = "AbsoluteErrorPropagation[function] returns the function and absolute error propagation.\AbsoluteErrorPropagation[function,{{p1,v1,e1},{p2,v2},p3,...}] calculates the function and gaussian error propagation given via the parameters.\AbsoluteErrorPropagation[function,{{p1,v1,e1},{p2,v2},p3},Analyze->True] calculates the function and absolute error propagation given via the parameters and analyzes the result.";
    AbsoluteErrorPropagation::lincount = "Count of linear named variables `1` does not match `2`";
    AbsoluteErrorPropagation::lininnlin = "Linear `1` cannot be in NonLinear construct.";
    AbsoluteErrorPropagation::nlininlin = "NonLinear `1` cannot be in Linear construct.";
    AbsoluteErrorPropagation::mulsymb = "The variables `1` are found multiple times in the parameter list.";
]; 



Begin["`Private`"] (* Begin Private Context *) 



AbsoluteErrorPropagation[function_,opts:OptionsPattern[]] :=
    With[ {propagationParameter = DeleteDuplicates[Cases[{function},_Symbol?((Context[#]==="Global`")&),Infinity]]},
        GaussianErrorPropagation[function,propagationParameter,opts]
    ]
AbsoluteErrorPropagation[function_,{param1_Symbol,param2_?NumericQ},opts:OptionsPattern[]] :=
    AbsoluteErrorPropagation[function,{{param1,param2}},opts]
AbsoluteErrorPropagation[function_,{param1_Symbol,param2_?NumericQ,param3_?NumericQ},opts:OptionsPattern[]] :=
    AbsoluteErrorPropagation[function,{{param1,param2,param3}},opts]


AbsoluteErrorPropagation[function_,parameters_?(MatchQ[#,List[(_Symbol|List[_Symbol]|List[_Symbol,_?NumericQ]|List[_Symbol,_Symbol]|List[_Symbol,_?NumericQ,_?NumericQ]|List[_Symbol,_Symbol,_?NumericQ]|_FittedModel|List[_FittedModel]|List[_FittedModel,List[_Symbol..]])..]]&),opts:OptionsPattern[]] :=
    With[ {i = Unique[],j = Unique[],fitmodobj = Unique[]},
        Module[ {parametersExpanded = parameters,uncorrelatedTerms,abort = False,outValue,subValues,algebraicTerms},
            
            (* substitute linearmodels by their variables *)
            parametersExpanded =
                (# /. 
                    {
                        List[fitmodobj_FittedModel,fitmodnms:List[_Symbol..]]
                    :>
                        If[ MatchQ[fitmodobj["BestFitParameters"],List[_?NumericQ..]],
                            If[ Length[fitmodobj["BestFitParameters"]]==Length[fitmodnms],
                                Apply[
                                    Sequence,
                                    Transpose[{fitmodnms,fitmodobj["BestFitParameters"],fitmodobj["ParameterErrors"]}]~Join~Flatten[Table[{fitmodnms[[i]],fitmodnms[[j]],fitmodobj["CovarianceMatrix"][[i,j]]},{i,1,Length[fitmodobj["BestFitParameters"]]},{j,i+1,Length[fitmodobj["BestFitParameters"]]}],1]
                                ]
                            ,
                                Message[GaussianErrorPropagation::lincount,fitmodnms,fitmodobj];
                                abort = True;
                            ]
                        ,
                            Message[GaussianErrorPropagation::nlininlin,fitmodobj];
                            abort = True;
                            fitmodobj
                        ]
                    }
                &) /@ parametersExpanded;
                
            (* substitute nonlinearmodels by their variables *) 
            parametersExpanded = 
               (# /.
                   {
                       List[fitmodobjl_FittedModel]:>fitmodobjl,fitmodobjl_FittedModel
                   :>
                       (If[
                           MatchQ[fitmodobjl["BestFitParameters"],List[Rule[_Symbol,_?NumericQ]..]]
                       ,
                           Apply[
                               Sequence
                           ,
                               Transpose[{Apply[Sequence,Transpose[fitmodobjl["BestFitParameters"]/.Rule->List]],fitmodobjl["ParameterErrors"]}]
                               ~Join~
                               Flatten[
                                   Table[
                                       {
                                           (fitmodobjl["BestFitParameters"]/.Rule->List)[[i,1]]
                                       ,
                                           (fitmodobjl["BestFitParameters"]/.Rule->List)[[j,1]]
                                       ,
                                           fitmodobjl["CovarianceMatrix"][[i,j]]
                                       }
                                   ,
                                       { i, 1, Length[ fitmodobjl["BestFitParameters"] ] }
                                   ,
                                       { j, i+1, Length[ fitmodobjl["BestFitParameters"] ] }
                                   ]
                               ,
                                   1
                               ]
                           ]
                       ,
                           Message[GaussianErrorPropagation::lininnlin,fitmodobjl];
                           abort = True;
                       ]
                       )
                   }& 
                ) /@ parametersExpanded;
            
            If[ abort, Return[] ];
            
            
            parametersExpanded = DeleteCases[parametersExpanded,Null,Infinity]; (* todo: simplify logic *)
            
            uncorrelatedTerms = (Select[parametersExpanded,If[ Head[#]===List,
                                                               If[ Length[#]>=2,
                                                                   If[ Head[#[[2]]]===Symbol,
                                                                       False,
                                                                       True
                                                                   ],
                                                                   True
                                                               ],
                                                               False
                                                           ]&][[All,1]])~Join~Select[parametersExpanded,Head[#]=!=List&];
            
            (* Detects doubled variable name *)
            If[ Length[uncorrelatedTerms]!=Length[DeleteDuplicates[uncorrelatedTerms]],
                Message[GaussianErrorPropagation::mulsymb,ToString[Select[Gather[uncorrelatedTerms],Length[#]>1&][[All,1]]]];
                Return[];
            ];
            
            (* Error Propation *)
            outValue =
                With[ 
                    { propagationParameterRules = (#[[1]]->#[[2]])& /@ Select[parametersExpanded,If[ Length[#]>=2,
                                                                                                           NumericQ[#[[2]]],
                                                                                                           False
                                                                                                       ]&]
                    }
                ,
                    {
                        function/.propagationParameterRules,
                        Total[
                        subValues = Table[
                            If[ Head[parametersExpanded[[i]]]===Symbol,
                               (((D[function,parametersExpanded[[i]]])/.propagationParameterRules)*C[parametersExpanded[[i]]])^2
                            ,
                                If[ Length[parametersExpanded[[i]]]==3,
                                    If[ NumericQ[parametersExpanded[[i,2]]],
                                       Abs[((D[function,parametersExpanded[[i,1]]])/.propagationParameterRules)*parametersExpanded[[i,3]]]
                                    ,
                                       0 (*  ( (2*D[function,parametersExpanded[[i,1]]]*D[function,parametersExpanded[[i,2]]]) /. propagationParameterRules) * parametersExpanded[[i,3]] *)
                                    ]
                                ,
                                    If[ Length[parametersExpanded[[i]]]==2,
                                        If[ NumericQ[parametersExpanded[[i,2]]],
                                            Abs[(((D[function,parametersExpanded[[i,1]]])/.propagationParameterRules)*C[parametersExpanded[[i,1]]])]
                                        ,
                                            0 (*(((2*D[function,parametersExpanded[[i,1]]]*D[function,parametersExpanded[[i,2]]])/.propagationParameterRules)*C[parametersExpanded[[i,1]],parametersExpanded[[i,2]]])*)
                                        ]
                                    ,
                                        Abs[ ((D[function,parametersExpanded[[i,1]]]) /. propagationParameterRules) * C[parametersExpanded[[i,1]]] ]
                                    ]
                                ]
                            ]
                        ,
                            { i, 1, Length[parametersExpanded] }
                        ]
                        ]
                    }
                ];
                
            (* Analyze Table Start*)
            If[ TrueQ[OptionValue[Analyze]],
                algebraicTerms = Table[
                    If[ Head[parametersExpanded[[i]]]===Symbol,
                        Abs[D[function,parametersExpanded[[i]]]*C[parametersExpanded[[i]]]]
                    ,
                        If[ Length[parametersExpanded[[i]]]>=2,
                            If[ Head[parametersExpanded[[i,2]]]===Symbol,
                                0 (*"Bielefeld"*) (*2*D[function,parametersExpanded[[i,1]]]*D[function,parametersExpanded[[i,2]]]*C[parametersExpanded[[i,1]],parametersExpanded[[i,2]]]*)
                            ,
                                Abs[D[function,parametersExpanded[[i,1]]]*Subscript["u", parametersExpanded[[i,1]]]]
                            ]
                        ,
                            Abs[D[function,parametersExpanded[[i,1]]]*Subscript["u", parametersExpanded[[i,1]]]]
                        ]
                    ]
                ,{i,1,Length[parametersExpanded]}];
                Print[
                    Grid[{
                        (* { Grid[{{"Parameters",parametersExpanded}}, Frame->All, Background->{{LightGray,White},None} ] } , *)
                        {
                            Grid[{
                                    {"Function","Uncertainty"}
                                ,
                                    {function,Total[algebraicTerms]}
                                ,
                                    {outValue[[1]],outValue[[2]]}
                                }
                            ,
                            Frame->All,Background->{None,{LightGray,White,LightGray}}]}
                        ,
                        {Grid[
                            {{"", "Mean", "Uncertainty", "Formula","Value","Weights [%]","Relative Weights [%]","Sqrt-Weights [%]"}}
                            ~Join~
                            Transpose[Transpose@Table[{
                                If[ Head[parametersExpanded[[i]]]=!=List,
                                    Subscript["u", parametersExpanded[[i]]]
                                ,
                                    If[ Head[parametersExpanded[[i,2]]]===Symbol,
                                        Subscript["u", parametersExpanded[[i,1]],parametersExpanded[[i,2]]]
                                    ,
                                        Subscript["u", parametersExpanded[[i,1]]]
                                    ]
                                ]
                            ,
                                If[ Head[parametersExpanded[[i]]]=!=List,
                                    parametersExpanded[[i]]
                                ,
                                    If[ Head[parametersExpanded[[i,2]]]===Symbol,
                                        0
                                    ,
                                        parametersExpanded[[i,2]]
                                    ]
                                ]
                            ,
                                If[ Head[parametersExpanded[[i]]]=!=List,
                                    Subscript["u", parametersExpanded[[i]]]
                                ,
                                   parametersExpanded[[i,3]]
                                ]
                            ,
                                algebraicTerms[[i]]
                            ,
                                subValues[[i]]
                            ,
                                100*subValues[[i]]/Total[Abs@subValues]
                            ,
                                100*subValues[[i]]/Total[subValues]}
                            ,
                                {i,1,Length[algebraicTerms]
                            }]
                            ~Join~
                            { (Sign/@subValues)*100*SqrtWeights[Abs@subValues]}]
                        ,
                            Frame->All, Background->{None,Table[If[ Mod[i,2]==0,White,LightGray],{i,1,Length[parametersExpanded]+1}]}
                        ]}
                    },Frame->All,Background->{None,{LightGray,White,LightGray}}]
                ]
            ];
            (* Analyze Table End *)
            
            If[ NumericQ[outValue[[2]]],
                Return@{outValue[[1]],Abs[outValue[[2]]]},
                Return@outValue
            ]
        ]
    ] 


End[] (* End Private Context *)

EndPackage[]