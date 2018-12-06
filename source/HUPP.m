(* ::Package:: *)

(* :Title: HUPP *)

(* :Author: Julien Kluge *)

(* :Summary:
This package introduces some command for handling
calculations for uncertanties, error-propagation and
plotting fitted model.
*)

(* :Package Version: 0.0.1 *)

BeginPackage["HUPP`"]

If[ Not@ValueQ[HUPPHelp::usage],
    HUPPHelp::usage = "HUPPHelp[] Lists all commands of the HUPP package."
];

If[ Not@ValueQ[StudentTValue::usage],
    StudentTValue::usage = "StudentTValue[dof] calculates the the student-t-value for the degrees of freedom dof and 1 sigma.\nStudentTValue[dof,\[Sigma]] calculates the student-t-value for the degrees of freedom and standard deviation \[Sigma].";
    StudentTValue::noninteger = "The parameter \.b4dof\.b4 should be an integer; Rounded `1` to `2`";
];

If[ Not@ValueQ[ConfidenceSigma::usage],
    ConfidenceSigma::usage = "ConfidenceSigma[\[Sigma]] calculates the statistic percentage of \[Sigma] standarddeviations."
];

If[ !ValueQ[InverseConfidenceSigma::usage],
    InverseConfidenceSigma::usage = "InverseConfidenceSigma[p] calculates the standard deviation of a statistical percentage."
];

If[ !ValueQ[ErrorProbability::usage],
    ErrorProbability::usage = "ErrorProbability[dof, \[Chi]P] calculates the error probability for a given degrees of freedom dof and chi-squared-value \[Chi]P.";
    ErrorProbability::xhippos = "The parameter \[Chi]P must be greater than 0.";
    ErrorProbability::noint = "The parameter \.b4dof\.b4 must be an integer; Rounded `1` to `2`";
];

Options[GaussianErrorPropagation] = {Analyze->False};
If[ !ValueQ[GaussianErrorPropagation::usage],
    GaussianErrorPropagation::usage = "GaussianErrorPropagation[function] returns the function and gaussian error propagation.\nGaussianErrorPropagation[function,{{p1,v1,e1},{p2,v2},p3,...}] calculates the function and gaussian error propagation given via the parameters.\nGaussianErrorPropagation[function,{{p1,v1,e1},{p2,v2},p3},Analyze->True] calculates the function and gaussian error propagation given via the parameters and analyzes the result.";
    GaussianErrorPropagation::lincount = "Count of linear named variables `1` does not match `2`";
    GaussianErrorPropagation::lininnlin = "Linear `1` cannot be in NonLinear construct.";
    GaussianErrorPropagation::nlininlin = "NonLinear `1` cannot be in Linear construct.";
    GaussianErrorPropagation::mulsymb = "The variables `1` are found multiple times in the parameter list.";
];

Options[WeightedMean] = {Analyze->False};
If[ !ValueQ[WeightedMean::usage],
    WeightedMean::usage = "WeightedMean[{{p1,e1},...}] calculates the weighted mean of a list.\nWeightedMean[{{p1,e1},...},Analyze->True] calculates the weighted mean of a list and analyzes the result.";
    WeightedMean::intervnooverl = "Error intervals do not overlap. A weighted mean should not applied on these values.";
];

Options[RoundedErrorForm] = {SignificantPower->Null,Form->""};
If[ !ValueQ[RoundedErrorForm::usage],
    RoundedErrorForm::usage = "RoundedErrorForm[{value,error}] returns the rounded error form of a value with its uncertanty."
];

Options[FitPlot] = Options[internFitPlot] = {FunctionNames->{},PlotRange->{},InitialGuesses->{},AdaptiveNonlinearFit->False,Residuals->False,ParameterNames->{},GridLines->None,FileSave->"",Units->{},ExtraPlots->{},ExtraLegends->Null,Analyze->True,RSquaredTest->True,ChiSquareTest->True,PlotMarkerSize->6};
If[ !ValueQ[FitPlot::usage],
    FitPlot::usage = "FitPlot[data,xErrors,yErrors,equations,variables,xLabel,yLabel,legendPosition] performs weighted equation-fits on data according to the variables and plots the result with labels and legends.\nFitPlot[data,equations,variables] performs equation-fits on data according to the variables and plots the result.\nFitPlot[data,yErrors,equations,variables] performs weighted equation-fits on data according to the variables and plots the result.\nFitPlot[data,xErrors,yErrors,equations,variables] performs weighted equation-fits on data according to the variables and plots the result.\nFitPlot[data,equations,variables,xLabel,yLabel] performs equation-fits on data according to the variables and plots the result with labels.\nFitPlot[data,equations,variables,xLabel,yLabel,legendPosition] performs equation-fits on data according to the variables and plots the result with labels and legends.\nFitPlot[data,yErrors,equations,variables,xLabel,yLabel,legendPosition] performs weighted equation-fits on data according to the variables and plots the result with labels and legends.";
    FitPlot::datawrong = "The data-parameter is in a wrong format or not numeric. Should be {{x,y},{x,y},...} or {{{x,y},{x,y},...},{{x,y},{x,y},...},...}";
    FitPlot::equationscount = "The equations-count, does not match the number of data-lists.";
    FitPlot::variablescount = "The variables-count, does not match the number of data-lists.";
    FitPlot::xerrorscount = "The xErrors-count, does not match the number of data-lists.";
    FitPlot::xerrorslistcount = "The xErrors-count, does not match the number of data-points in list.";
    FitPlot::xerrorswrong = "The xErrors-parameter is in a wrong format or not numeric. Should be Null, {u1,u2,...} or {{u1,u2,...},{u1,u2,...},...}.";
    FitPlot::yerrorscount = "The yErrors-count, does not match the number of data-lists.";
    FitPlot::yerrorslistcount = "The yErrors-count, does not match the number of data-points in list.";
    FitPlot::yerrorswrong = "The yErrors-parameter is in a wrong format or not numeric. Should be Null, {u1,u2,...} or {{u1,u2,...},{u1,u2,...},...}.";
    FitPlot::yerrorsnull = "The yErrors-parameter contains a zero (0). Weighting cannot be processed.";
    FitPlot::functionvarcount = "The FunctionNames-option, does not match the number of data-lists.";
    FitPlot::functionvarwrong = "The FunctionNames-option is in a wrong format. Should be {\"..\",\"..\",...}";
    FitPlot::plotrangewrong = "The PlotRange-option is in a wrong format or not numeric. Should be {} or {{x1,x2},{y1,y2}}";
    FitPlot::initialguesseswrong = "The InitialGuesses-option is in a wrong format or not numeric. Should be {}, {x->y,x->y,...} or {{x->y,x->y,...},{x->y,x->y,...},...}";
    FitPlot::initialguessecount = "The InitialGuesses-option, does not match the number of data-lists.";
    FitPlot::paramnameswrong = "The ParameterNames-option is in a wrong format. Should be {}, {x->y,x->y,...} or {{x->y,x->y,...},{x->y,x->y,...},...}";
    FitPlot::paramnamescount = "The ParameterNames-option, does not match the number of data-lists.";
    FitPlot::unitscount = "The Units-option, does not match the number of data-lists or count of variables.";
    FitPlot::unitswrong = "The Units-option is in a wrong format. Should be {}, {\"..\",\"..\",...} or {{\"..\",\"..\",...},{\"..\",\"..\",...},...}";
    FitPlot::illeheads = "The equations-parameter contains list-constructs.";
    FitPlot::rsqrdcount = "The RSquaredTest-option, does not match the number of data-lists.";
    FitPlot::chisqrdcount = "The ChiSquareTest-option, does not match the number of data-lists.";
];

If[ !ValueQ[PythagoreanSum::usage],
    PythagoreanSum::usage = "PythagoreanSum[values] calculates the pytagorean sum of multiple values."
];

If[ !ValueQ[ConfidenceInterval::usage],
    ConfidenceInterval::usage = "ConfidenceInterval[values] calculates the studentized confidence interval."
];

If[ !ValueQ[ConfidenceIntervalHU::usage],
    ConfidenceIntervalHU::usage = "ConfidenceIntervalHU[values] calculates the confidence interval as defined in the HU-physics-practical."
];

Options[CalculateMeasurement] = {Analyze->False};
If[ !ValueQ[CalculateMeasurement::usage],
    CalculateMeasurement::usage = "CalculateMeasurement[{v1,v2,...},{e1,e2*C,...}] calculates a whole measurement with statistic uncertanty.\nCalculateMeasurement[{v1,v2,...},{e1,e2*C,...},esys] calculates a whole measurement with statistic uncertanty and a systematic error.\nCalculateMeasurement[{v1,v2,...},{e1,e2*C,...},esys,Analyze->True] calculates a whole measurement with statistic uncertanty, a systematic error and analyzes the result."
];

Options[CalculateMeasurementHU] = {Analyze->False};
If[ !ValueQ[CalculateMeasurementHU::usage],
    CalculateMeasurementHU::usage = "CalculateMeasurement[{v1,v2,...},{e1,e2*C,...}] calculates a whole measurement with statistic uncertanty as defined in the HU-physics-practical.\nCalculateMeasurement[{v1,v2,...},{e1,e2*C,...},esys] calculates a whole measurement with statistic uncertanty and a systematic error as defined in the HU-physics-practical.\nCalculateMeasurement[{v1,v2,...},{e1,e2*C,...},esys,Analyze->True] calculates a whole measurement with statistic uncertanty, a systematic error as defined in the HU-physics-practical and analyzes the result."
];

If[ !ValueQ[ErrorMean::usage],
    ErrorMean::usage = "ErrorMean[errors] calculates the mean of errors.\nErrorMean[{{v1,e1},...}] calculates the mean of the values and errors.";
];

Options[AdaptiveNonlinearModelFit] = Options[NonlinearModelFit];
If[ !ValueQ[AdaptiveNonlinearModelFit::usage],
    AdaptiveNonlinearModelFit::usage = "AdaptiveNonlinearModelFit[data,form,parameters,x] constructs a nonlinear model to fit the form to the given data while trying to choose appropriate starting values.";
];


Begin["`Private`"]


HUPPHelp[] :=
    Information["HUPP`*"]


SqrtWeights[u_List] :=
    ((Sqrt[u])/(Sqrt[Total[u]]))/Total[(Sqrt[u])/(Sqrt[Total[u]])]


ConfidenceSigma[\[Sigma]_?NumericQ] :=
    Erf[\[Sigma]/Sqrt[2]]


InverseConfidenceSigma[p_?NumericQ] :=
    InverseErf[p]*Sqrt[2]


StudentTValue[dof_] :=
    StudentTValue[dof,1]
StudentTValue[dof_,\[Sigma]_] :=
    InverseCDF[StudentTDistribution[If[ IntegerQ[dof],
                                        dof,
                                        Message[StudentTValue::noninteger,dof,Round[dof]];
                                        Round[dof]
                                    ]],(1+Erf[\[Sigma]/Sqrt[2]])/2]


ErrorProbability[dof_,\[Chi]P_] :=
    (1-CDF[ChiSquareDistribution[If[ IntegerQ[dof],
                                     dof,
                                     Message[ErrorProbability::noint,dof,Round[dof]];
                                     Round[dof]
                                 ]],\[Chi]P])/;(If[ \[Chi]P<=0,
                                                    Message[ErrorProbability::xhippos]
                                                ];
                                                \[Chi]P>0)


GaussianErrorPropagation[function_,opts:OptionsPattern[]] :=
    With[ {propagationParameter = DeleteDuplicates[Cases[{function},_Symbol?((Context[#]==="Global`")&),Infinity]]},
        GaussianErrorPropagation[function,propagationParameter,opts]
    ]
GaussianErrorPropagation[function_,{param1_Symbol,param2_?NumericQ},opts:OptionsPattern[]] :=
    GaussianErrorPropagation[function,{{param1,param2}},opts]
GaussianErrorPropagation[function_,{param1_Symbol,param2_?NumericQ,param3_?NumericQ},opts:OptionsPattern[]] :=
    GaussianErrorPropagation[function,{{param1,param2,param3}},opts]
GaussianErrorPropagation[function_,parameters_?(MatchQ[#,List[(_Symbol|List[_Symbol]|List[_Symbol,_?NumericQ]|List[_Symbol,_Symbol]|List[_Symbol,_?NumericQ,_?NumericQ]|List[_Symbol,_Symbol,_?NumericQ]|_FittedModel|List[_FittedModel]|List[_FittedModel,List[_Symbol..]])..]]&),opts:OptionsPattern[]] :=
    With[ {i = Unique[],j = Unique[],fitmodobj = Unique[]},
        Module[ {parametersExpanded = parameters,uncorrelatedTerms,abort = False,outValue,subValues,algebraicTerms},
            parametersExpanded = (#/.{List[fitmodobj_FittedModel,fitmodnms:List[_Symbol..]]:>If[ MatchQ[fitmodobj["BestFitParameters"],List[_?NumericQ..]],
                                                                                                 If[ Length[fitmodobj["BestFitParameters"]]==Length[fitmodnms],
                                                                                                     Apply[Sequence,Transpose[{fitmodnms,fitmodobj["BestFitParameters"],fitmodobj["ParameterErrors"]}]~Join~Flatten[Table[{fitmodnms[[i]],fitmodnms[[j]],fitmodobj["CovarianceMatrix"][[i,j]]},{i,1,Length[fitmodobj["BestFitParameters"]]},{j,i+1,Length[fitmodobj["BestFitParameters"]]}],1]],
                                                                                                     Message[GaussianErrorPropagation::lincount,fitmodnms,fitmodobj];
                                                                                                     abort = True;
                                                                                                 ],
                                                                                                 Message[GaussianErrorPropagation::nlininlin,fitmodobj];
                                                                                                 abort = True;
                                                                                                 fitmodobj
                                                                                             ]}&)/@parametersExpanded;
            parametersExpanded = (#/.{List[fitmodobjl_FittedModel]:>fitmodobjl,fitmodobjl_FittedModel:>(
            If[ MatchQ[fitmodobjl["BestFitParameters"],List[Rule[_Symbol,_?NumericQ]..]],
                Apply[Sequence,Transpose[{Apply[Sequence,Transpose[fitmodobjl["BestFitParameters"]/.Rule->List]],fitmodobjl["ParameterErrors"]}]~Join~Flatten[Table[{(fitmodobjl["BestFitParameters"]/.Rule->List)[[i,1]],(fitmodobjl["BestFitParameters"]/.Rule->List)[[j,1]],fitmodobjl["CovarianceMatrix"][[i,j]]},{i,1,Length[fitmodobjl["BestFitParameters"]]},{j,i+1,Length[fitmodobjl["BestFitParameters"]]}],1]],
                Message[GaussianErrorPropagation::lininnlin,fitmodobjl];
                abort = True;
            ]
            )}&)/@parametersExpanded;
            If[ abort,
                Return[]
            ];
            parametersExpanded = DeleteCases[parametersExpanded,Null,Infinity];
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
            If[ Length[uncorrelatedTerms]!=Length[DeleteDuplicates[uncorrelatedTerms]],
                Message[GaussianErrorPropagation::mulsymb,ToString[Select[Gather[uncorrelatedTerms],Length[#]>1&][[All,1]]]];
                Return[];
            ];
            outValue = With[ {propagationParameterRules = (#[[1]]->#[[2]])&/@Select[parametersExpanded,If[ Length[#]>=2,
                                                                                                           NumericQ[#[[2]]],
                                                                                                           False
                                                                                                       ]&]},
                           {
                           function/.propagationParameterRules,
                           Sqrt[
                           Total[
                           subValues = Table[
                           If[ Head[parametersExpanded[[i]]]===Symbol,
                               (((D[function,parametersExpanded[[i]]])/.propagationParameterRules)*C[parametersExpanded[[i]]])^2,
                               If[ Length[parametersExpanded[[i]]]==3,
                                   If[ NumericQ[parametersExpanded[[i,2]]],
                                       (((D[function,parametersExpanded[[i,1]]])/.propagationParameterRules)*parametersExpanded[[i,3]])^2,
                                       ((2*D[function,parametersExpanded[[i,1]]]*D[function,parametersExpanded[[i,2]]])/.propagationParameterRules)*parametersExpanded[[i,3]]
                                   ],
                                   If[ Length[parametersExpanded[[i]]]==2,
                                       If[ NumericQ[parametersExpanded[[i,2]]],
                                           (((D[function,parametersExpanded[[i,1]]])/.propagationParameterRules)*C[parametersExpanded[[i,1]]])^2,
                                           (((2*D[function,parametersExpanded[[i,1]]]*D[function,parametersExpanded[[i,2]]])/.propagationParameterRules)*C[parametersExpanded[[i,1]],parametersExpanded[[i,2]]])
                                       ],
                                       (((D[function,parametersExpanded[[i,1]]])/.propagationParameterRules)*C[parametersExpanded[[i,1]]])^2
                                   ]
                               ]
                           ]
                           ,{i,1,Length[parametersExpanded]}]
                           ]
                           ]
                           }
                       ];
            If[ TrueQ[OptionValue[Analyze]],
                algebraicTerms = Table[
                If[ Head[parametersExpanded[[i]]]===Symbol,
                    (D[function,parametersExpanded[[i]]]*C[parametersExpanded[[i]]])^2,
                    If[ Length[parametersExpanded[[i]]]>=2,
                        If[ Head[parametersExpanded[[i,2]]]===Symbol,
                            2*D[function,parametersExpanded[[i,1]]]*D[function,parametersExpanded[[i,2]]]*C[parametersExpanded[[i,1]],parametersExpanded[[i,2]]],
                            (D[function,parametersExpanded[[i,1]]]*C[parametersExpanded[[i,1]]])^2
                        ],
                        (D[function,parametersExpanded[[i,1]]]*C[parametersExpanded[[i,1]]])^2
                    ]
                ]
                ,{i,1,Length[parametersExpanded]}];
                Print[Grid[{
                {Grid[{{"Parameters",parametersExpanded}},Frame->All,Background->{{LightGray,White},None}]},
                {Grid[{
                {"Function","Uncertanty"},
                {function,Sqrt[Total[algebraicTerms]]},{outValue[[1]],outValue[[2]]}
                },Frame->All,Background->{None,{LightGray,White,LightGray}}]},
                {Grid[{{"Type/Variable","Formula","Value","Weights [%]","Relative Weights [%]","Sqrt-Weights [%]"}}~Join~Transpose[Transpose@Table[{If[ Head[parametersExpanded[[i]]]=!=List,
                                                                                                                                                        C[parametersExpanded[[i]]],
                                                                                                                                                        If[ Head[parametersExpanded[[i,2]]]===Symbol,
                                                                                                                                                            C[parametersExpanded[[i,1]],parametersExpanded[[i,2]]],
                                                                                                                                                            C[parametersExpanded[[i,1]]]
                                                                                                                                                        ]
                                                                                                                                                    ],algebraicTerms[[i]],subValues[[i]],100*subValues[[i]]/Total[Abs@subValues],100*subValues[[i]]/Total[subValues]},{i,1,Length[algebraicTerms]}]~Join~{(Sign/@subValues)*100*SqrtWeights[Abs@subValues]}],Frame->All,Background->{None,Table[If[ Mod[i,2]==0,
                                                                                                                                                                                                                                                                                                                                                                                                    White,
                                                                                                                                                                                                                                                                                                                                                                                                    LightGray
                                                                                                                                                                                                                                                                                                                                                                                                ],{i,1,Length[parametersExpanded]+1}]}]}
                },Frame->All,Background->{None,{LightGray,White,LightGray}}
                ]]
            ];
            If[ NumericQ[outValue[[2]]],
                Return@{outValue[[1]],Abs[outValue[[2]]]},
                Return@outValue
            ]
        ]
    ]


WeightedMean[values_?(MatchQ[#,List[List[_,_]..]]&),opts:OptionsPattern[]] :=
    (If[ Not[And@@Flatten[Table[If[ values[[i,1]]>values[[j,1]],
                                    (values[[i,1]]-values[[i,2]])<=(values[[j,1]]+values[[j,2]]),
                                    (values[[i,1]]+values[[i,2]])>=(values[[j,1]]-values[[j,2]])
                                ],{i,1,Length[values]},{j,i+1,Length[values]}]]],
         Message[WeightedMean::intervnooverl]
     ];
     If[ OptionValue[Analyze],
         Module[ {weights = 1/(values[[All,2]]^2),mean = Total[values[[All,1]]/(values[[All,2]]^2)]/Total[1/(values[[All,2]]^2)],meanError = 1/Sqrt[Total[1/(values[[All,2]]^2)]]},
             Print@Grid[
             {
             {Grid[{{"\!\(\*OverscriptBox[\(x\), \(_\)]\)","\!\(\*OverscriptBox[\(u\), \(_\)]\)","\[CapitalSigma] p","\[CapitalSigma] p\[CenterDot]x"},{mean,meanError,Total[weights],Total[values[[All,1]]*weights]}},Frame->All,Background->{None,{LightGray,White}}]},
             {
             Grid[{
             {"values x"}~Join~values[[All,1]],
             {"uncertanties u"}~Join~values[[All,2]],
             {"weights p"}~Join~weights,
             {"weight-portion [%]"}~Join~(weights/Total[weights]*100),
             {"p\[CenterDot]x"}~Join~(values[[All,1]]*weights),
             {"p\[CenterDot]x [%]"}~Join~(values[[All,1]]*weights/Total[values[[All,1]]*weights]*100),
             {"Sqrt-p\[CenterDot]x [%]"}~Join~(SqrtWeights[values[[All,1]]*weights]*100)
             },Frame->All,Background->{None,{LightGray,White,LightGray,White,LightGray,White}}]
             },
             {
             Show[
             NumberLinePlot[{{Interval[mean+{-1,1}*meanError],Total[values[[All,1]]/(values[[All,2]]^2)]/Total[1/(values[[All,2]]^2)]}}~Join~({{Interval[#[[1]]+{-1,1}*#[[2]]],#[[1]]}}&/@values),ImageSize->800,PlotRange->{{0.99,1.01}*MinMax[(values[[All,1]]+values[[All,2]])~Join~(values[[All,1]]-values[[All,2]])],All},PlotTheme->"Scientific",AspectRatio->1/5,PlotStyle->({Black}~Join~Table[Hue[Mod[GoldenRatio*(i-1),1],1,0.7],{i,1,Length[values]}])]
             ,Graphics[{Black,InfiniteLine[{{mean,0},{mean,1}}],Gray,InfiniteLine[{{mean+meanError,0},{mean+meanError,1}}],InfiniteLine[{{mean-meanError,0},{mean-meanError,1}}]}]]
             }
             },Frame->All,Background->{None,{White,LightGray,White}}]
         ]
     ];
     Return@{Total[values[[All,1]]/(values[[All,2]]^2)]/Total[1/(values[[All,2]]^2)],1/Sqrt[Total[1/(values[[All,2]]^2)]]})


RoundedErrorForm[{value_,error_},opts:OptionsPattern[]] :=
    RoundedErrorForm[value,error,opts]
RoundedErrorForm[value_?NumericQ,error_?NumericQ,opts:OptionsPattern[]] :=
    Module[ {vPower,ePower,power,trimedValue,trimedError,roundingDigits,vString,eString,vFormattedString,eFormattedString,outString},
        vPower = Floor[Log[10,Abs[value]]];
        ePower = Floor[Log[10,Abs[error]]];
        power = Max[vPower,ePower];
        If[ OptionValue[SignificantPower]===Null,
            roundingDigits = Min[vPower,ePower]-power;
            If[ Floor[Abs[error]/(10^ePower)]<3,
                roundingDigits--
            ];(*DIN1333*),
            roundingDigits = OptionValue[SignificantPower]
        ];
        trimedValue = Floor[Abs[value]/(10^power)*10^(-roundingDigits)+0.5]10^(roundingDigits);
        trimedError = Ceiling[Abs[error]/(10^power)*10^(-roundingDigits)]10^(roundingDigits);
        If[ Log[10,Abs[trimedValue]]>=1,
            trimedValue/=10;
            trimedError/=10;
            power+=1;
            roundingDigits-=1;
        ];(*10 overtaking*)
        vString = RealDigits[trimedValue//N,10,Abs[roundingDigits]+1,0]/.{Indeterminate->0};
        eString = RealDigits[trimedError//N,10,Abs[roundingDigits]+1,0]/.{Indeterminate->0};
        vFormattedString = StringJoin@@(ToString[#]&/@vString[[1]]);
        eFormattedString = StringJoin@@(ToString[#]&/@eString[[1]]);
        If[ roundingDigits!=0,
            vFormattedString = StringInsert[vFormattedString,".",2];
            eFormattedString = StringInsert[eFormattedString,".",2];
        ];
        If[ value<0,
            vFormattedString = StringInsert[vFormattedString,"-",1]
        ];
        If[ TrueQ[OptionValue[Form]==Null],
            Return[{trimedValue*10^power,trimedError*10^power}];,
            If[ ToUpperCase[OptionValue[Form]]=="TEX"||ToUpperCase[OptionValue[Form]]=="LATEX",
                outString = "\\left("<>vFormattedString<>"\\pm"<>eFormattedString<>"\\right)";
                If[ power!=0,
                    outString = outString<>"10^{"<>ToString[power]<>"}"
                ];,
                outString = "("<>vFormattedString<>"\[PlusMinus]"<>eFormattedString<>")";
                If[ power!=0,
                    outString = outString<>ToString[Superscript[10,power],TraditionalForm]
                ];
            ];
            Return[outString];
        ];
    ]


(*https://github.com/AlexeyPopkov/PolygonPlotMarkers/*)
ClearAll[PolygonArea,PolygonCentroid,LineIntersectionPoint,ngon,nstar,ncross,scale,coords];
PolygonArea[pts_?MatrixQ] :=
    Abs@Total[Det/@Partition[pts,2,1,1]]/2;
PolygonCentroid[pts_?MatrixQ] :=
    With[ {dif = Map[Det,Partition[pts,2,1,{1,1}]]},
        ListConvolve[{{1,1}},Transpose[pts],{-1,-1}].dif/(3 Total[dif])
    ];
LineIntersectionPoint[{a_,b_},{c_,d_}] :=
    (Det[{a,b}] (c-d)-Det[{c,d}] (a-b))/Det[{a-b,c-d}];

ngon[n_,phase_:0] :=
    Table[{0,1}.RotationMatrix[2k Pi/n+phase],{k,0,n-1}];
nstar[n_/;n>=5,phase_:0] :=
    nstar[n,2,n,phase];
nstar[nn_,step_,n_,phase_:0]/;Divisible[nn,n]&&nn/2>step>nn/n :=
    Module[ {a1,a2,b1,b2,ab},
        {a1,a2,b1,b2} = ngon[nn][[{1,1+step,1+nn/n,nn/n-step}]];
        ab = LineIntersectionPoint[{a1,a2},{b1,b2}];
        Flatten[Table[{a1,ab}.RotationMatrix[2k Pi/n+phase],{k,0,n-1}],1]
    ];
ncross[n_,phase_:0,a_:1/10] :=
    Flatten[NestList[#.RotationMatrix[2Pi/n]&,{{-a,1},{a,1},{a,a Cot[Pi/n]}}.RotationMatrix[phase],n-1],1];

scale[coords_] :=
    Chop[#/Sqrt@PolygonArea@#]&@N[coords,{18,18}];

coords["UpTriangle"|"Triangle"] = ngon[3]//scale;
coords["DownTriangle"] = ngon[3,Pi/3]//scale;
coords["LeftTriangle"] = ngon[3,Pi/6]//scale;
coords["RightTriangle"] = ngon[3,-Pi/6]//scale;
coords["ThreePointedStar"] = nstar[12,5,3]//scale;
coords["DiagonalSquare"|"Diamond"] = ngon[4,0]//scale;
coords["Square"] = ngon[4,Pi/4]//scale;
coords["FourPointedStar"] = nstar[8,3,4]//scale;
coords["DiagonalFourPointedStar"] = nstar[8,3,4,Pi/4]//scale;
coords["Pentagon"] = ngon[5]//scale;
coords["FivePointedStar"] = nstar[5]//scale;
coords["FivePointedStarThick"] = nstar[20,7,5]//scale;
coords["Hexagon"] = ngon[6]//scale;
coords["SixPointedStar"] = nstar[6]//scale;
coords["SixPointedStarSlim"] = nstar[12,5,6]//scale;
coords["SevenPointedStar"] = nstar[7]//scale;
coords["SevenPointedStarNeat"] = nstar[14,5,7]//scale;
coords["SevenPointedStarSlim"] = nstar[14,6,7]//scale;
coords["Cross"|"+"] = ncross[4]//scale;
coords["DiagonalCross"|"CrossDiagonal"|"X"|"x"] = ncross[4,Pi/4]//scale;
coords["TripleCross"|"TripleCrossUp"] = ncross[3]//scale;
coords["TripleCrossDown"|"Y"|"y"] = ncross[3,Pi/3]//scale;
coords["FivefoldCross"] = ncross[5]//scale;
coords["SixfoldCross"] = ncross[6]//scale;
coords["SevenfoldCross"] = ncross[7]//scale;
coords["EightfoldCross"] = ncross[8]//scale;
coords["UpTriangleTruncated"|"TriangleTruncated"|"TruncatedTriangle"] = Flatten[{{-3,6+Sqrt[3]},{3,6+Sqrt[3]}}.RotationMatrix[# Pi/3]&/@{0,2,4},1]//scale;
coords["DownTriangleTruncated"] = coords["UpTriangleTruncated"].ReflectionMatrix[{0,1}];
coords["LeftTriangleTruncated"] = coords["UpTriangleTruncated"].RotationMatrix[Pi/6];
coords["RightTriangleTruncated"] = coords["UpTriangleTruncated"].RotationMatrix[-Pi/6];
coords["Disk"|"Circle"] = ngon[24]//scale;

PolygonMarker[name_String,(h:Scaled|Offset)[size_?NumericQ]] :=
    Polygon[h[size #,{0,0}]&/@coords[name]];
PolygonMarker[] = PolygonMarker[All] = {"TripleCross","Y","UpTriangle","UpTriangleTruncated","DownTriangle","DownTriangleTruncated","LeftTriangle","LeftTriangleTruncated","RightTriangle","RightTriangleTruncated","ThreePointedStar","Cross","DiagonalCross","Diamond","Square","FourPointedStar","DiagonalFourPointedStar","FivefoldCross","Pentagon","FivePointedStar","FivePointedStarThick","SixfoldCross","Hexagon","SixPointedStar","SixPointedStarSlim","SevenfoldCross","SevenPointedStar","SevenPointedStarNeat","SevenPointedStarSlim","EightfoldCross","Disk"};
PolygonPlotMarker[name_,color_,size_: 8] :=
    Graphics[{Dynamic@EdgeForm@Directive[color,JoinForm["Round"],AbsoluteThickness[1],Opacity[1]],FaceForm[Opacity[0.15,color]],PolygonMarker[name,Offset[size]]},AlignmentPoint->{0,0}]


RoundedFitPlotErrorForm[value_?NumericQ, error_?NumericQ, 
  opts : OptionsPattern[]] :=
    Module[ {vPower, ePower, power, trimedValue, trimedError, 
      roundingDigits, vString, eString, vFormattedString, 
      eFormattedString, outString},
        vPower = Floor[Log[10, Abs[value]]];
        ePower = Floor[Log[10, Abs[error]]];
        power = Max[vPower, ePower];
        roundingDigits = Min[vPower, ePower] - power;
        If[ Floor[Abs[error]/(10^ePower)] < 3,
            roundingDigits--
        ];(*DIN1333*)
        trimedValue = 
         Floor[Abs[value]/(10^power)*10^(-roundingDigits) + 
            0.5] 10^(roundingDigits);
        trimedError = 
         Ceiling[Abs[
              error]/(10^power)*10^(-roundingDigits)] 10^(roundingDigits);
        If[ Log[10, Abs[trimedValue]] >= 1,
            trimedValue /= 10;
            trimedError /= 10;
            power += 1;
            roundingDigits -= 1;
        ];(*10 overtaking*)
        vString = 
         RealDigits[trimedValue // N, 10, Abs[roundingDigits] + 1, 
           0] /. {Indeterminate -> 0};
        eString = 
         RealDigits[trimedError // N, 10, Abs[roundingDigits] + 1, 
           0] /. {Indeterminate -> 0};
        vFormattedString = StringJoin @@ (ToString[#] & /@ vString[[1]]);
        eFormattedString = StringJoin @@ (ToString[#] & /@ eString[[1]]);
        If[ roundingDigits != 0,
            vFormattedString = StringInsert[vFormattedString, ".", 2];
            eFormattedString = StringInsert[eFormattedString, ".", 2];
        ];
        If[ value < 0,
            vFormattedString = StringInsert[vFormattedString, "-", 1]
        ];
        outString = 
         "(" <> vFormattedString <> "\[PlusMinus]" <> eFormattedString <> 
          ")";
        If[ power != 0,
            outString = 
             outString <> ToString[Superscript[10, power], TraditionalForm]
        ];
        Return[outString];
    ]

ContainsOnlyVar[expr_, var_] :=
    With[ {symbolNDuplList = 
       DeleteDuplicates[
        Cases[{expr}, _Symbol?((Context[#] === "Global`") &), 
         Infinity]]},
        If[ Length[symbolNDuplList] == 1,
            TrueQ[symbolNDuplList[[1]] == var],
            False
        ]
    ]

FreeOfVarAndSymb[expr_, var_] :=
    With[ {symbolNDuplList = 
       DeleteDuplicates[
        Cases[{expr}, _Symbol?((Context[#] === "Global`") &), 
         Infinity]]},
        If[ Length[symbolNDuplList] < 1,
            False,
            ! MemberQ[symbolNDuplList, var]
        ]
    ]

TestSingleTimesExpression[expr_, var_] :=
    If[ Head[expr] === Times,
        If[ MatchQ[expr, 
          Times[_Symbol?(FreeOfVarAndSymb[#, var] &), _?(ContainsOnlyVar[#, 
               var] &)]],
            Flatten[Cases[{expr}, 
              Times[symb_Symbol?(FreeOfVarAndSymb[#, var] &), 
                func_?(ContainsOnlyVar[#, var] &)] :> {symb, func}]],
            False
        ],
        False
    ]

TestMultiPlusExpression[expr_, var_] :=
    If[ Head[expr] === Plus,
        With[ {plusExprDuplList = (If[ Head[#] === Symbol,
                                       If[ FreeOfVarAndSymb[#, var],
                                           {#, 1},
                                           False
                                       ],
                                       TestSingleTimesExpression[#, var]
                                   ]) & /@ Apply[List, expr]},
            If[ MemberQ[Flatten[plusExprDuplList], False],
                False,
                If[ Length[DeleteDuplicates[plusExprDuplList[[All, 1]]]] == 
                  Length[plusExprDuplList[[All, 1]]],
                    If[ Length[DeleteDuplicates[plusExprDuplList[[All, 2]]]] == 
                      Length[plusExprDuplList[[All, 2]]],
                        plusExprDuplList,
                        False
                    ],
                    False
                ]
            ]
        ],
        False
    ]

FitPlot[data_List,equations_,variables_,opts:OptionsPattern[]] :=
    internFitPlot[data,{},{},equations,variables,"","",{},opts]
FitPlot[data_List,yErrors_List,equations_?(Head[#]=!=String&),variables_,opts:OptionsPattern[]] :=
    internFitPlot[data,{},yErrors,equations,variables,"","",{},opts]
FitPlot[data_List,xErrors_List,yErrors_List,equations_?(Head[#]=!=String&),variables_,opts:OptionsPattern[]] :=
    internFitPlot[data,xErrors,yErrors,equations,variables,"","",{},opts]
FitPlot[data_List,equations_,variables_,xLabel_String,yLabel_String,legendPosition:(_?(#==Null&)|_List),opts:OptionsPattern[]] :=
    internFitPlot[data,{},{},equations,variables,xLabel,yLabel,legendPosition,opts]
FitPlot[data_List,yErrors_List,equations_,variables_,xLabel_String,yLabel_String,legendPosition:(_?(#==Null&)|_List),opts:OptionsPattern[]] :=
    internFitPlot[data,{},yErrors,equations,variables,xLabel,yLabel,legendPosition,opts]
FitPlot[data_List,equations_,variables_,xLabel_String,yLabel_String,opts:OptionsPattern[]] :=
    internFitPlot[data,{},{},equations,variables,xLabel,yLabel,{},opts]
FitPlot[data_List,yErrors_List,equations_,variables_,xLabel_String,yLabel_String,opts:OptionsPattern[]] :=
    internFitPlot[data,{},yErrors,equations,variables,xLabel,yLabel,{},opts]
FitPlot[data_List,xErrors_List,yErrors_List,equations_,variables_,xLabel_String,yLabel_String,opts:OptionsPattern[]] :=
    internFitPlot[data,xErrors,yErrors,equations,variables,xLabel,yLabel,{},opts]
FitPlot[data_List,xErrors_List,yErrors_List,equations_,variables_,xLabel_String,yLabel_String,legendPosition:(_?(#==Null&)|_List),opts:OptionsPattern[]] :=
    internFitPlot[data,xErrors,yErrors,equations,variables,xLabel,yLabel,legendPosition,opts]
internFitPlot[data_List,xErrors_List,yErrors_List,equations_,variables_,xLabel_String,yLabel_String,legendPosition:(_?(#==Null&)|_List),opts:OptionsPattern[]] :=
    With[ {i = Unique[],j = Unique[]},
        Module[ {abort,pointList,equationList,variableList,coefficientsList,coefficientNames,coefficientStringNames,fitHelpData,count,fits,fitTypes,linearFunctions,basePlot,errorPlots,residualsPlot,plotRange,residualRange,dashingList,xErrorList,yErrorList,legend,legendPos,outObject,functionStrings,InitialGuessesData,InitialCoefficients,unitList,infoGrid,paramNames,saveDir,systemInfo,ePlots,eLegends,subParameter,rsquareTestList,chisquareTestList,stringMatches,choosenPlotMarker,choosenPlotMarkerSize,adaptiveFit},
            abort = False; (*get your shit together mathematica...return is return...*)
            If[ MatchQ[data,List[List[_?NumericQ,_?NumericQ]..]], (*check and format the data parameter*)
                pointList = {data};
                count = 1;,
                If[ MatchQ[data,List[List[List[_?NumericQ,_?NumericQ]..]..]],
                    pointList = data;
                    count = Length[data];,
                    Message[FitPlot::datawrong];
                    Return[];
                ];
            ];
            InitialGuessesData = OptionValue[InitialGuesses]; (*this has to come early, since the equations must possibly alter this*)
            If[ TrueQ[InitialGuessesData=={}],
                InitialGuessesData = Table[{},{i,1,count}];,
                If[ MatchQ[InitialGuessesData,List[Rule[_Symbol|I|C|K|_String,_?NumericQ]..]],
                    InitialGuessesData = {InitialGuessesData};
                ]; (*two birds with one stone*)
                If[ MatchQ[InitialGuessesData,List[List[Rule[_Symbol|I|C|K|_String,_?NumericQ]...]..]],
                    If[ Length[InitialGuessesData]!=count,
                        Message[FitPlot::initialguessecount];
                        InitialGuessesData = Table[{},{i,1,count}];
                    ];,
                    Message[FitPlot::initialguesseswrong];
                    InitialGuessesData = Table[{},{i,1,count}];
                ];
            ];
            If[ TrueQ[OptionValue[ParameterNames]=={}],
                paramNames = Table[{},{i,1,count}], (*this has to come early, since the equations must possibly alter this*)
                paramNames = OptionValue[ParameterNames];
                If[ MatchQ[paramNames,List[Rule[_Symbol,_]...]],
                    paramNames = {paramNames};
                ];
                If[ MatchQ[paramNames,List[List[Rule[_Symbol,_]...]..]],
                    If[ Length[paramNames]!=count,
                        Message[FitPlot::paramnamescount];
                        Return[];
                    ];,
                    Message[FitPlot::paramnameswrong];
                    Return[];
                ];
            ];
            If[ Head[equations]===List, (*check and format the equations parameter*)
                If[ Length[equations]==count,
                    equationList = equations;,
                    Message[FitPlot::equationscount];
                    Return[];
                ];,
                equationList = ConstantArray[equations,count];
            ];
            If[ Head[variables]===List, (*check and format variables parameter*)
                If[ Length[variables]==count,
                    variableList = variables;,
                    Message[FitPlot::variablescount];
                    Return[];
                ];,
                variableList = ConstantArray[variables,count];
            ];
            Do[
            If[ TrueQ[variableList[[i]]==I],
                subParameter = Unique["I"];
                variableList[[i]] = subParameter;
                equationList[[i]] = equationList[[i]]/.I->subParameter;
                paramNames[[i]] = paramNames[[i]]~Join~{subParameter->"I"};
            ];
            If[ TrueQ[variableList[[i]]==C],
                subParameter = Unique["C"];
                variableList[[i]] = subParameter;
                equationList[[i]] = equationList[[i]]/.C->subParameter;
                paramNames[[i]] = paramNames[[i]]~Join~{subParameter->"C"};
            ];
            If[ TrueQ[variableList[[i]]==K],
                subParameter = Unique["K"];
                variableList[[i]] = subParameter;
                equationList[[i]] = equationList[[i]]/.K->subParameter;
                paramNames[[i]] = paramNames[[i]]~Join~{subParameter->"K"};
            ];
            ,{i,1,count}];
            Do[
            If[ Not@TrueQ[variableList[[i]]==I],
                If[ Not@FreeQ[equationList[[i]],I],
                    subParameter = Unique["I"];
                    equationList[[i]] = equationList[[i]]/.I->subParameter;
                    If[ FreeQ[paramNames[[i]],I],
                        paramNames[[i]] = paramNames[[i]]~Join~{subParameter->"I"};,
                        paramNames[[i]] = paramNames[[i]]/.I->subParameter;
                    ];
                    InitialGuessesData[[i]] = InitialGuessesData[[i]]/.I->subParameter;
                ];
            ];
,{i,1,count}];
            Do[
            If[ Not@TrueQ[variableList[[i]]==C],
                If[ Not@FreeQ[equationList[[i]],C],
                    subParameter = Unique["C"];
                    equationList[[i]] = equationList[[i]]/.C->subParameter;
                    If[ FreeQ[paramNames[[i]],C],
                        paramNames[[i]] = paramNames[[i]]~Join~{subParameter->"C"};,
                        paramNames[[i]] = paramNames[[i]]/.C->subParameter;
                    ];
                    InitialGuessesData[[i]] = InitialGuessesData[[i]]/.C->subParameter;
                ];
            ];
,{i,1,count}];
            Do[
            If[ Not@TrueQ[variableList[[i]]==K],
                If[ Not@FreeQ[equationList[[i]],K],
                    subParameter = Unique["K"];
                    equationList[[i]] = equationList[[i]]/.K->subParameter;
                    If[ FreeQ[paramNames[[i]],K],
                        paramNames[[i]] = paramNames[[i]]~Join~{subParameter->"K"};,
                        paramNames[[i]] = paramNames[[i]]/.K->subParameter;
                    ];
                    InitialGuessesData[[i]] = InitialGuessesData[[i]]/.K->subParameter;
                ];
            ];
,{i,1,count}];
            If[ Not@(And@@(FreeQ[#,_List]&/@equationList)),
                Message[FitPlot::illeheads];
                Return[];
            ];
            Do[ (*string compensation*)
            stringMatches = DeleteDuplicates[Cases[equationList[[i]],_String,\[Infinity]]];
            If[ Length[stringMatches]>0,
                Do[
                subParameter = Unique[stringMatches[[j]]];
                equationList[[i]] = equationList[[i]]/.((stringMatches[[j]])->subParameter);
                If[ FreeQ[paramNames[[i]],stringMatches[[j]]],
                    paramNames[[i]] = paramNames[[i]]~Join~{subParameter->(stringMatches[[j]])};,
                    paramNames[[i]] = paramNames[[i]]/.((stringMatches[[j]])->subParameter);
                ];
                InitialGuessesData[[i]] = InitialGuessesData[[i]]/.((stringMatches[[j]])->subParameter);
                If[ variableList[[i]]==stringMatches[[j]],
                    variableList[[i]] = subParameter;
                ];
                ,{j,1,Length[stringMatches]}];
            ];
            ,{i,1,count}];
            
            If[ TrueQ[xErrors=={}], (*check and format xErrors-Parameter*)
                xErrorList = Table[0,{i,1,count},{j,1,Length[pointList[[i]]]}];,
                If[ MatchQ[xErrors,List[_?NumericQ..]],
                    If[ count==1,
                        If[ Length[xErrors]==Length[pointList[[1]]],
                            xErrorList = {xErrors};,
                            Message[FitPlot::xerrorslistcount];
                            Return[];
                        ];,
                        Message[FitPlot::xerrorscount];
                        Return[];
                    ];,
                    If[ MatchQ[xErrors,List[List[_?NumericQ...]..]],
                        If[ Length[xErrors]==count,
                            xErrorList = Table[
                            If[ Length[xErrors[[i]]]==Length[pointList[[i]]],
                                xErrors[[i]],
                                If[ TrueQ[xErrors[[i]]=={}],
                                    Table[0,{j,1,Length[pointList[[i]]]}],
                                    Message[FitPlot::xerrorslistcount];
                                    abort = True;
                                ]
                            ]
                            ,{i,1,count}];,
                            Message[FitPlot::xerrorscount];
                            Return[];
                        ],
                        Message[FitPlot::xerrorswrong];
                        Return[];
                    ];
                ];
            ];
            If[ abort,
                Return[];
            ]; (*really?...*)
            If[ TrueQ[yErrors=={}], (*check and format yErrors-Parameter*)
                yErrorList = Table[0,{i,1,count},{j,1,Length[pointList[[i]]]}];,
                If[ MatchQ[yErrors,List[_?NumericQ..]],
                    If[ count==1,
                        If[ Length[yErrors]==Length[pointList[[1]]],
                            If[ MemberQ[yErrors,0],
                                Message[FitPlot::yerrorsnull];
                                Return[];,
                                yErrorList = {yErrors};
                            ],
                            Message[FitPlot::yerrorslistcount];
                            Return[];
                        ];,
                        Message[FitPlot::yerrorscount];
                        Return[];
                    ];,
                    If[ MatchQ[yErrors,List[List[_?NumericQ...]..]],
                        If[ Length[yErrors]==count,
                            yErrorList = Table[
                            If[ Length[yErrors[[i]]]==Length[pointList[[i]]],
                                If[ MemberQ[yErrors[[i]],0],
                                    Message[FitPlot::yerrorsnull];
                                    abort = True;,
                                    yErrors[[i]]
                                ],
                                If[ TrueQ[yErrors[[i]]=={}],
                                    Table[0,{j,1,Length[pointList[[i]]]}],
                                    Message[FitPlot::yerrorslistcount];
                                    abort = True;
                                ]
                            ]
                            ,{i,1,count}];,
                            Message[FitPlot::yerrorscount];
                            Return[];
                        ],
                        Message[FitPlot::yerrorswrong];
                        Return[];
                    ];
                ];
            ];
            If[ abort,
                Return[];
            ];

            (*FromCharacterCode[ToCharacterCode["f"]-1+i]<>"("<>ToString[variableList[[i]]/.paramNames[[i]],TraditionalForm]<>")"*)
            functionStrings = Table["",{i,1,count}]; (*function pre strings f(x)...*)
            If[ MatchQ[OptionValue[FunctionNames],List[(_)..]],
                If[ Length[OptionValue[FunctionNames]]==count,
                    Do[
                    If[ Head[OptionValue[FunctionNames][[j]]]===String,
                        functionStrings[[j]] = OptionValue[FunctionNames][[j]];,
                        If[ !TrueQ[OptionValue[FunctionNames][[j]]=={}],
                            Message[FitPlot::functionvarwrong];
                            Return[];
                        ];
                    ];
,{j,1,count}];,
                    Message[FitPlot::functionvarcount];
                    Return[];
                ];,
                If[ !TrueQ[OptionValue[FunctionNames]=={}],
                    Message[FitPlot::functionvarwrong];
                    Return[];
                ];
            ];
            ePlots = {};
            If[ !TrueQ[OptionValue[ExtraPlots]=={}],
                If[ Head[OptionValue[ExtraPlots]]===List,
                    ePlots = OptionValue[ExtraPlots];,
                    ePlots = {OptionValue[ExtraPlots]};
                ];
            ];
            eLegends = {};
            If[ !TrueQ[OptionValue[ExtraLegends]=={}],
                If[ Head[OptionValue[ExtraLegends]]===List,
                    If[ And@@((Head[#]===List)&/@OptionValue[ExtraLegends]),
                        eLegends = OptionValue[ExtraLegends];,
                        eLegends = {OptionValue[ExtraLegends]};
                    ];,
                    eLegends = {{OptionValue[ExtraLegends]}};
                ];
            ];
            rsquareTestList = OptionValue[RSquaredTest];
            If[ Head[rsquareTestList]===List,
                If[ Length[rsquareTestList]!=count,
                    Message[FitPlot::rsqrdcount];
                    Return[];
                ];,
                If[ rsquareTestList==True||rsquareTestList==False,
                    rsquareTestList = ConstantArray[rsquareTestList,count]
                ];
            ];
            chisquareTestList = OptionValue[ChiSquareTest];
            If[ Head[chisquareTestList]===List,
                If[ Length[chisquareTestList]!=count,
                    Message[FitPlot::chisqrdcount];
                    Return[];
                ];,
                If[ chisquareTestList==True||chisquareTestList==False,
                    chisquareTestList = ConstantArray[chisquareTestList,count]
                ];
            ];
            choosenPlotMarker = PolygonMarker[All][[{31,3,15,14,4,5,7,19,17,23,13,11,20,10,26}[[1;;count]]]];
            choosenPlotMarkerSize = OptionValue[PlotMarkerSize];
            adaptiveFit = OptionValue[AdaptiveNonlinearFit];
            fitHelpData = ConstantArray[0,count];
            fitTypes = ConstantArray[0,count]; (*determine fit types (0: polynomial linear, 1: nonlinear, 2: simple linear, 3: complex linear)*)
            Do[
            If[ PolynomialQ[equationList[[i]],variableList[[i]]],
                coefficientsList = CoefficientList[equationList[[i]],variableList[[i]]];
                If[ And@@(MatchQ[#,(_Symbol|0)]&/@coefficientsList),
                    If[ Length[DeleteDuplicates[coefficientsList]]==Length[coefficientsList],
                        fitTypes[[i]] = 0;,
                        fitTypes[[i]] = 1;
                    ];,
                    fitTypes[[i]] = 1;
                ];,
                fitHelpData[[i]] = TestSingleTimesExpression[equationList[[i]],variableList[[i]]];
                If[ BooleanQ[fitHelpData[[i]]],
                    fitHelpData[[i]] = TestMultiPlusExpression[equationList[[i]],variableList[[i]]];
                    If[ BooleanQ[fitHelpData[[i]]],
                        fitTypes[[i]] = 1;,
                        fitTypes[[i]] = 3;
                    ];,
                    fitTypes[[i]] = 2;
                ];
            ];
,{i,1,count}];
            coefficientNames = ConstantArray[0,count];
            fits = Table[ (*make fits*)
            If[ fitTypes[[i]]==0,
                coefficientsList = CoefficientList[equationList[[i]],variableList[[i]]];
                linearFunctions = DeleteCases[(If[ #[[2]]=!=0,
                                                   variableList[[i]]^#[[1]],
                                                   0
                                               ])&/@Transpose[{Range[0,Length[coefficientsList]-1],coefficientsList}],0];
                coefficientNames[[i]] = DeleteCases[coefficientsList,0];
                If[ MemberQ[yErrorList[[i]],0],
                    LinearModelFit[pointList[[i]],linearFunctions,variableList[[i]],IncludeConstantBasis->False],
                    LinearModelFit[pointList[[i]],linearFunctions,variableList[[i]],IncludeConstantBasis->False,Weights->(1/((yErrorList[[i]])^2))]
                ],
                If[ fitTypes[[i]]==1,
                    coefficientsList = DeleteCases[DeleteDuplicates[
                    With[ {globalQ = ((Context[#]==="Global`")&)},
                        Cases[Level[equationList[[i]],{-1}],_Symbol?globalQ,Infinity]
                    ]
                    ],variableList[[i]]];
                    coefficientNames[[i]] = coefficientsList;
                    InitialCoefficients = Table[If[ MemberQ[InitialGuessesData[[i,All,1]],coefficientsList[[j]]],
                                                    {coefficientsList[[j]],coefficientsList[[j]]/.InitialGuessesData[[i]]},
                                                    coefficientsList[[j]]
                                                ],{j,1,Length[coefficientsList]}];
                    If[ MemberQ[yErrorList[[i]],0],
                        If[ adaptiveFit,
                            AdaptiveNonlinearModelFit[pointList[[i]],equationList[[i]],InitialCoefficients,variableList[[i]]],
                            NonlinearModelFit[pointList[[i]],equationList[[i]],InitialCoefficients,variableList[[i]],MaxIterations->10000]
                        ],
                        If[ adaptiveFit,
                            AdaptiveNonlinearModelFit[pointList[[i]],equationList[[i]],InitialCoefficients,variableList[[i]],Weights->(1/((yErrorList[[i]])^2))],
                            NonlinearModelFit[pointList[[i]],equationList[[i]],InitialCoefficients,variableList[[i]],Weights->(1/((yErrorList[[i]])^2)),MaxIterations->10000]
                        ]
                    ],
                    If[ fitTypes[[i]]==2,
                        coefficientNames[[i]] = {fitHelpData[[i,1]]};
                        LinearModelFit[pointList[[i]],fitHelpData[[i,2]],variableList[[i]],IncludeConstantBasis->False],
                        coefficientNames[[i]] = fitHelpData[[i,All,1]];
                        LinearModelFit[pointList[[i]],fitHelpData[[i,All,2]],variableList[[i]],IncludeConstantBasis->False]
                    ]
                ]
            ]
            ,{i,1,count}];
            If[ TrueQ[OptionValue[Units]=={}],
                unitList = Table[Table["",{j,1,Length[coefficientNames[[i]]]}],{i,1,count}];,
                If[ Head[OptionValue[Units]]===String||OptionValue[Units]===Symbol,
                    unitList = Table[Table[If[ Head[OptionValue[Units]]===String,
                                               OptionValue[Units],
                                               ToString[OptionValue[Units]]
                                           ],{j,1,Length[pointList[[i]]]}],{i,1,count}];,
                    unitList = OptionValue[Units];
                    If[ MatchQ[unitList,List[(_String|_Symbol)..]],
                        If[ count==1&&Length[unitList]==Length[coefficientNames[[1]]],
                            unitList = {((If[ Head[#]===String,
                                              #,
                                              ToString[#]
                                          ]&)/@unitList)};,
                            Message[FitPlot::unitscount];
                            Return[];
                        ];,
                        If[ MatchQ[unitList,List[List[(_String|_Symbol)...]..]],
                            If[ Length[unitList]==count,
                                unitList = Table[If[ TrueQ[unitList[[i]]=={}],
                                                     Table["",{j,1,Length[coefficientNames[[i]]]}],
                                                     If[ Length[unitList[[i]]]==Length[coefficientNames[[i]]],
                                                         If[ Head[#]===String,
                                                             #,
                                                             ToString[#]
                                                         ]&/@(unitList[[i]]),
                                                         Message[FitPlot::unitscount];
                                                         abort = True;
                                                     ]
                                                 ],{i,1,count}];,
                                Message[FitPlot::unitscount];
                                Return[];
                            ];,
                            Message[FitPlot::unitswrong];
                            Return[];
                        ]
                    ];
                ];
            ];
            If[ abort,
                Return[];
            ];
            coefficientStringNames = Table[If[ Head[#]===String||Head[#]===Style,
                                               #,
                                               ToString[#,TraditionalForm]
                                           ]&/@(coefficientNames[[i]]/.(paramNames[[i]])),{i,1,count}];
            plotRange = {{Min[Flatten[pointList[[All,All,1]]]-Flatten[xErrorList]],Max[Flatten[pointList[[All,All,1]]]+Flatten[xErrorList]]} (*fits*)
            ,{Min[Flatten[pointList[[All,All,2]]]-Flatten[yErrorList]],Max[Flatten[pointList[[All,All,2]]]+Flatten[yErrorList]]}}; (*absolute range*)
            plotRange = {{plotRange[[1,1]]-0.02*Abs[plotRange[[1,1]]-plotRange[[1,2]]],plotRange[[1,2]]+0.02*Abs[plotRange[[1,1]]-plotRange[[1,2]]]},
            {plotRange[[2,1]]-0.02*Abs[plotRange[[2,1]]-plotRange[[2,2]]],plotRange[[2,2]]+0.02*Abs[plotRange[[2,1]]-plotRange[[2,2]]]}}; (*rescale with 2% absolute scale to gather a neat border*)
            If[ TrueQ[OptionValue[PlotRange]!={}],
                If[ MatchQ[OptionValue[PlotRange],List[List[_?NumericQ,_?NumericQ],List[_?NumericQ,_?NumericQ]]],
                    plotRange = OptionValue[PlotRange];,
                    Message[FitPlot::plotrangewrong];
                ]
            ];
            dashingList = {Dashing[{}],Dashed,DotDashed,Dotted}~Join~(Dashing/@({{Medium,Small,Large,Small},{Large,Small}}))~Join~(Dashing/@Tuples[{0,Medium,Small},3]); (*dashing list*)
            
            Needs["ErrorBarPlots`"]; (*error plots*)
            errorPlots = Table[

            	ErrorBarPlots`ErrorListPlot[
            		N@Table[
            			{pointList[[j,i]],ErrorBarPlots`ErrorBar[xErrorList[[j,i]],yErrorList[[j,i]]]},{i,1,Length[pointList[[j]]]}
            		],
            		PlotRange->plotRange,
            		PlotTheme->"Scientific",
            		ImageSize->Large,
            		PlotStyle->Directive[{ColorData[97][j]}],
            		PlotMarkers->{
            			(PolygonPlotMarker[#,ColorData[97][j], choosenPlotMarkerSize]&)
            			@(choosenPlotMarker[[j]]),
            			choosenPlotMarkerSize
            		}		
            	],
            	{j,count,1,-1}
            ];
            basePlot = Show[ (*base plots*)
            Table[Plot[fits[[i]]["BestFit"],{variableList[[i]]}~Join~plotRange[[1]],PlotRange->plotRange,PlotTheme->"Scientific",ImageSize->Large,PlotStyle->Directive[{ColorData[97][i],dashingList[[i]]}]],{i,1,count}]~Join~errorPlots
            ,FrameLabel->{Style[xLabel,{FontSize->16,FontTracking->"Extended"}],Style[yLabel,{FontSize->16,FontTracking->"Extended"}]},GridLines->OptionValue[GridLines],FrameTicksStyle->14];
            If[ TrueQ[ePlots!={}],
                basePlot = Show[{basePlot}~Join~ePlots]
            ];
            If[ OptionValue[Residuals],
                residualRange = {-1.1*#,1.1*#}&@Max[Flatten[Table[Max[Abs[fits[[i]]["FitResiduals"]]+Abs[yErrorList[[i]]]],{i,1,count}]]];
                residualsPlot = Show[Table[ErrorBarPlots`ErrorListPlot[MapThread[{#1,ErrorBarPlots`ErrorBar[#2]}&,{Transpose[{pointList[[i,All,1]],fits[[i]]["FitResiduals"]}],yErrorList[[i]]}],PlotTheme->"Scientific",ImageSize->Large,AspectRatio->1/5,PlotRange->{plotRange[[1]],residualRange},PlotStyle->Directive[{ColorData[97][i]}],FrameLabel->{None,Style["residuals",{FontSize->16,FontTracking->"Extended"}]},FrameTicks->{{Automatic,None},{Automatic,All}},Mesh->All,Joined->True,PlotMarkers->{PolygonPlotMarker[#,ColorData[97][i],choosenPlotMarkerSize]&@(choosenPlotMarker[[i]])}],{i,1,count}],FrameTicksStyle->14];
            ];
            legend = LineLegend[Table[Directive[{ColorData[97][i],dashingList[[i]]}],{i,1,count}], (*legend creation*)Table[If[ functionStrings[[i]]=="",
                                                                                                                                "",
                                                                                                                                functionStrings[[i]]<>"="
                                                                                                                            ]<>ToString[equationList[[i]]/.(paramNames[[i]]),TraditionalForm]<>If[ chisquareTestList[[i]],
                                                                                                                                                                                                   "\n\!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\)/dof="<>ToString[fits[[i]]["ANOVATableMeanSquares"][[-1]],TraditionalForm],
                                                                                                                                                                                                   ""
                                                                                                                                                                                               ]<>If[ fitTypes[[i]]==1||Not[rsquareTestList[[i]]],
                                                                                                                                                                                                      "",
                                                                                                                                                                                                      "\n\!\(\*SuperscriptBox[OverscriptBox[\(R\), \(_\)], \(2\)]\)="<>ToString[fits[[i]]["AdjustedRSquared"],TraditionalForm]
                                                                                                                                                                                                  ]<>"\n"<>StringJoin[Riffle[Table[coefficientStringNames[[i,j]]<>"="<>RoundedFitPlotErrorForm[(fits[[i]]["BestFitParameters"]/.Rule[a_,b_]:>b)[[j]],(fits[[i]]["ParameterErrors"])[[j]]]<>If[ unitList[[i,j]]=="",
                                                                                                                                                                                                                                                                                                                                                                                               "",
                                                                                                                                                                                                                                                                                                                                                                                               " "<>unitList[[i,j]]
                                                                                                                                                                                                                                                                                                                                                                                           ],{j,1,Length[coefficientStringNames[[i]]]}],"\n"]],{i,1,count}],LegendMarkers->(PolygonPlotMarker[choosenPlotMarker[[#]],ColorData[97][#],choosenPlotMarkerSize]&/@Range[count]),LegendMarkerSize->{60,25},LegendMargins->{{10, 0}, {20, 10}},LabelStyle->13];
            If[ TrueQ[eLegends!={}],
                legend = Grid[{{legend}}~Join~eLegends]
            ];
            If[ TrueQ[legendPosition=={}], (*outObject preparation*)
                outObject = Legended[If[ OptionValue[Residuals],
                                         Grid[{{basePlot},{residualsPlot}}],
                                         basePlot
                                     ],legend];,
                If[ MatchQ[legendPosition,{_?NumericQ,_?NumericQ}],
                    outObject = Show[basePlot,Epilog->{Inset[Framed[legend,Background->Opacity[0.8,White],FrameStyle->Gray],legendPosition,{Left,Top}]}];,
                    outObject = basePlot;
                ];
                If[ OptionValue[Residuals],
                    outObject = Grid[{{outObject},{residualsPlot}}];
                ];
            ];
            infoGrid = Grid[Transpose[{Item[Style[#,{FontColor->White}],Background->GrayLevel[0.4]]&/@({"regression","equation","parameters","model-object","{\!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\)/dof,\[Alpha]}","{\!\(\*SuperscriptBox[\(R\), \(2\)]\),\!\(\*SuperscriptBox[OverscriptBox[\(R\), \(~\)], \(2\)]\)}\n{\!\(\*SuperscriptBox[\(R\), \(2\)]\)%,\!\(\*SuperscriptBox[OverscriptBox[\(R\), \(~\)], \(2\)]\)%}","correlations","covariances"}~Join~If[ OptionValue[Residuals],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                       {},
                                                                                                                                                                                                                                                                                                                                                                                                                                                                       {"residuals"}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ])}~Join~Transpose[{
            Table[LineLegend[{Directive[{ColorData[97][i],dashingList[[i]]}]},{Switch[fitTypes[[i]],0,"polynomial linear",1,If[ adaptiveFit,
                                                                                                                                "adaptive ",
                                                                                                                                ""
                                                                                                                            ]<>"nonlinear",2,"single linear",3,"multi-combination linear"]}],{i,1,count}],
            
            Table[If[ TrueQ[paramNames[[i]]=={}],
                      TraditionalForm[equationList[[i]]],
                      {Style[TraditionalForm[equationList[[i]]],{FontColor->Orange}],TraditionalForm[equationList[[i]]/.paramNames[[i]]]}
                  ],{i,1,count}],
            
            Table[Grid[{{Item["",Background->GrayLevel[0.7]],"value","error","p-value",Item["",Background->GrayLevel[0.7]]}}~Join~Transpose[{
            Item[#,Background->GrayLevel[0.7]]&/@coefficientNames[[i]],
            fits[[i]]["BestFitParameters"]/.Rule[a_,b_]:>b,
            fits[[i]]["ParameterErrors"],
            fits[[i]]["ParameterPValues"],
            Item[#,Background->GrayLevel[0.7]]&/@coefficientStringNames[[i]]
            }],Frame->All,Background->{None,{White}~Join~Table[If[ (fits[[i]]["ParameterPValues"])[[j]]<=0.05,
                                                                   LightGreen,
                                                                   LightRed
                                                               ],{j,1,Length[fits[[i]]["ParameterPValues"]]}]}
            ],{i,1,count}],
            
            Table[fits[[i]],{i,1,count}],
            
            Table[With[ {kd = 1-CDF[ChiSquareDistribution[fits[[i]]["ANOVATableDegreesOfFreedom"][[2]]],fits[[i]]["ANOVATableMeanSquares"][[-1]]]},
                      Style[{fits[[i]]["ANOVATableMeanSquares"][[-1]],N[kd]},FontColor->If[ kd>=1/2,
                                                                                            RGBColor[0,0.7,0],
                                                                                            If[ kd<6/100,
                                                                                                Red,
                                                                                                Orange
                                                                                            ]
                                                                                        ]]
                  ],{i,1,count}],
            
            Table[Style[ToString[{fits[[i]]["RSquared"],fits[[i]]["AdjustedRSquared"]},TraditionalForm]<>"\n"<>ToString[N[100*{1-Sqrt[1-(fits[[i]]["RSquared"])],1-Sqrt[1-(fits[[i]]["AdjustedRSquared"])]}],TraditionalForm],{FontColor->If[ 1-Sqrt[1-(fits[[i]]["AdjustedRSquared"])]>=0.6826894921370859,
                                                                                                                                                                                                                                              If[ 1-Sqrt[1-(fits[[i]]["AdjustedRSquared"])]>=0.9544997361036416,
                                                                                                                                                                                                                                                  RGBColor[0,0.7,0],
                                                                                                                                                                                                                                                  Orange
                                                                                                                                                                                                                                              ],
                                                                                                                                                                                                                                              Red
                                                                                                                                                                                                                                          ]}],{i,1,count}],
            
            Table[Grid[{{""}~Join~((Item[#,Background->GrayLevel[0.7]]&)/@coefficientNames[[i]])~Join~{""}}~Join~Transpose[{coefficientNames[[i]]}~Join~Transpose[fits[[i]]["CorrelationMatrix"]]~Join~{coefficientStringNames[[i]]}]~Join~{{""}~Join~((Item[#,Background->GrayLevel[0.7]]&)/@coefficientStringNames[[i]])~Join~{""}},Frame->All,Background->{({GrayLevel[0.7]}~Join~ConstantArray[White,Length[coefficientNames[[i]]]]~Join~{GrayLevel[0.7]}),None}],{i,1,count}],
            
            Table[Grid[{{""}~Join~((Item[#,Background->GrayLevel[0.7]]&)/@coefficientNames[[i]])~Join~{""}}~Join~Transpose[{coefficientNames[[i]]}~Join~Transpose[fits[[i]]["CovarianceMatrix"]]~Join~{coefficientStringNames[[i]]}]~Join~{{""}~Join~((Item[#,Background->GrayLevel[0.7]]&)/@coefficientStringNames[[i]])~Join~{""}},Frame->All,Background->{({GrayLevel[0.7]}~Join~ConstantArray[White,Length[coefficientNames[[i]]]]~Join~{GrayLevel[0.7]}),None}],{i,1,count}]
            
            }~Join~If[ OptionValue[Residuals],
                       {},
                       {Table[ListLinePlot[fits[[i]]["FitResiduals"],AspectRatio->1/5,ImageSize->250,PlotTheme->"Minimal",FrameTicks->False,PlotRange->{{0,Length[fits[[i]]["FitResiduals"]]+1},{-#,#}&@Max[Abs[fits[[i]]["FitResiduals"]]]}],{i,1,count}]}
                   ]]],Frame->All,Background->{None,Table[If[ Mod[i,2]==1,
                                                              White,
                                                              LightGray
                                                          ],{i,1,8}]}];
            If[ OptionValue[FileSave]!="",
                If[ DirectoryName[OptionValue[FileSave]]=="",
                    saveDir = Quiet[Check[NotebookDirectory[],""]];
                    If[ saveDir=="",
                        saveDir = HomeDirectory[];
                        If[ StringTake[saveDir,-1]!="\\",
                            saveDir = saveDir<>"\\"
                        ];
                        If[ DirectoryQ[saveDir<>"Desktop\\"],
                            saveDir = saveDir<>"Desktop\\"<>OptionValue[FileSave];
                            Export[saveDir,outObject];,
                            If[ DirectoryQ[saveDir],
                                saveDir = saveDir<>OptionValue[FileSave];
                                Export[saveDir,outObject];,
                                saveDir = Style["not saved (no directory could be found)",FontColor->Red];
                            ];
                        ];,
                        If[ StringTake[saveDir,-1]!="\\",
                            saveDir = saveDir<>"\\"
                        ];
                        saveDir = saveDir<>OptionValue[FileSave];
                        Export[saveDir,outObject];
                    ];,
                    saveDir = OptionValue[FileSave];
                    Export[OptionValue[FileSave],outObject];
                ];,
                saveDir = Style["not saved (no file specified)",FontColor->Orange]
            ];
            systemInfo = Quiet[(If[ MatchQ[#,List[List[___]...]],
                                    "ScreenArea"/.(#[[1]]),
                                    "ScreenArea"/.#
                                ]&@SystemInformation["Devices","ScreenInformation"])[[1,2]]];
            If[ !TrueQ[OptionValue[Analyze]],
                Return[{fits,outObject}],
                If[ count<=If[ TrueQ[systemInfo>=1920],
                               2,
                               If[ TrueQ[systemInfo>=1080],
                                   1,
                                   0
                               ]
                           ],
                    Return[Grid[{{infoGrid,outObject},{saveDir}}]],
                    Return[Grid[{{infoGrid},{saveDir},{outObject}}]]
                ];
            ];
        ]
    ]


PythagoreanSum[values___] :=
    Sqrt[Total[Flatten[List[values]]^2]]


ConfidenceInterval[data__] :=
    With[ {internalData = Flatten[List[data]]},
        InverseCDF[StudentTDistribution[Length[internalData]-1],(1+Erf[1/Sqrt[2]])/2]*StandardDeviation[internalData]/Sqrt[Length[internalData]]
    ]


ConfidenceIntervalHU[data__] :=
    With[ {internalData = Flatten[List[data]]},
        If[ Length[internalData]<6,
            Max[Abs[internalData-Mean[internalData]]],
            StandardDeviation[internalData]/Sqrt[Length[internalData]]
        ]
    ]


CalculateMeasurement[measurements_List,opts:OptionsPattern[]] :=
    CalculateMeasurement[measurements,{0},0,opts]
CalculateMeasurement[measurements_List,u_List,opts:OptionsPattern[]] :=
    CalculateMeasurement[measurements,u,0,opts]
CalculateMeasurement[measurements_List,u_List,usys_,opts:OptionsPattern[]] :=
    (If[ OptionValue[Analyze],
         With[ {mmean = Mean[measurements],mmeansys = Mean[measurements]+usys,mn = Length[measurements],ms = StandardDeviation[measurements],uu = Abs[Flatten[u/.C:>(Mean[measurements]+usys)]],usum = Total[Abs[Flatten[u/.C:>(Mean[measurements]+usys)]]^2]+(InverseCDF[StudentTDistribution[Length[measurements]-1],(1+Erf[1/Sqrt[2]])/2]*StandardDeviation[measurements]/Sqrt[Length[measurements]])^2,ut = InverseCDF[StudentTDistribution[Length[measurements]-1],(1+Erf[1/Sqrt[2]])/2],np = Count[Mean[measurements]-measurements,_?Positive],nm = Count[Mean[measurements]-measurements,_?Negative]},
             Print@Grid[{
             {Grid[{{"\!\(\*OverscriptBox[\(x\), \(_\)]\)","\!\(\*SubscriptBox[\(u\), \(sys\)]\)","\!\(\*OverscriptBox[\(x\), \(_\)]\)+\!\(\*SubscriptBox[\(u\), \(sys\)]\)","u","\!\(\*SubscriptBox[\(u\), \(sys\)]\)/\!\(\*OverscriptBox[\(x\), \(_\)]\) [%]","n","\!\(\*SubscriptBox[\(n\), \(+\)]\)","\!\(\*SubscriptBox[\(n\), \(-\)]\)","\!\(\*SubscriptBox[\(n\), \(0\)]\)","|\!\(\*SubscriptBox[\(n\), \(+\)]\)-\!\(\*SubscriptBox[\(n\), \(-\)]\)|\[LessEqual]\!\(\*SqrtBox[\(n\)]\)","t","\[Sigma]","\!\(\*SubscriptBox[\(min\), \(i\)]\)|\!\(\*OverscriptBox[\(x\), \(_\)]\)-\!\(\*SubscriptBox[\(x\), \(i\)]\)|","\!\(\*SubscriptBox[\(max\), \(i\)]\)|\!\(\*OverscriptBox[\(x\), \(_\)]\)-\!\(\*SubscriptBox[\(x\), \(i\)]\)|"},{mmean,usys,mmeansys,Sqrt[usum],Abs[usys/mmean]*100,mn,np,nm,mn-(np+nm),Item[ToString[Abs[np-nm]]<>"\[LessEqual]"<>ToString[N[Sqrt[mn]]],Background->If[ TrueQ[Abs[np-nm]<=Sqrt[mn]],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      LightGreen,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      LightRed
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ]],N[ut],ms}~Join~MinMax[Abs[mmean-measurements]]},Frame->All,Background->{None,{LightGray,White}}]},
             {Grid[Transpose[{{"uncertanity u","N[u]","u^2","uncertanty weight [%]"}}~Join~{{"\!\(\*OverscriptBox[\(s\), \(_\)]\)=t\[CenterDot]s/\!\(\*SqrtBox[\(n\)]\)",N[ut*ms/Sqrt[mn]],N[(ut*ms/Sqrt[mn])^2],100*(ut*ms/Sqrt[mn])^2/usum}}~Join~Table[{u[[i]],uu[[i]],uu[[i]]^2,100*uu[[i]]^2/usum},{i,1,Length[u]}]]~Join~{{"Sqrt-uncertanty weights [%]"}~Join~(100*SqrtWeights[{N[(ut*ms/Sqrt[mn])^2]}~Join~uu^2])},Frame->All,Background->{None,{LightGray,White,LightGray,White}}]},
             {Show[Histogram[measurements+usys,Automatic,"PDF",PlotTheme->"Scientific",ImageSize->700,FrameTicks->{{Automatic,Automatic},{Automatic,{{mmeansys,"\[Mu]"}}~Join~Flatten[Table[{{i*ms+mmeansys,ToString[i]<>"\[Sigma]"},{-i*ms+mmeansys,"-"<>ToString[i]<>"\[Sigma]"}},{i,1,5}],1]}},AspectRatio->1/3,Epilog->{Gray,Thick,InfiniteLine[{{mmeansys,0},{mmeansys,1}}],Thin}~Join~Flatten[Table[{InfiniteLine[{{-i*ms+mmeansys,0},{-i*ms+mmeansys,1}}],InfiniteLine[{{i*ms+mmeansys,0},{i*ms+mmeansys,1}}]},{i,1,5}]],PlotRange->{{-5*ms+mmeansys,5*ms+mmeansys},All}],SmoothHistogram[measurements+usys,Automatic,"PDF",PlotRange->{{-5*ms+mmeansys,5*ms+mmeansys},All},PlotStyle->Red],Plot[Evaluate@PDF[NormalDistribution[mmeansys,ms],x],{x,-5*ms+mmeansys,5*ms+mmeansys},PlotStyle->Blue,PlotRange->{{-5*ms+mmeansys,5*ms+mmeansys},All}]]}
             },Frame->All,Background->{None,{White,LightGray}}]
         ]
     ];
     Return@{Mean[measurements]+usys,Sqrt[Total[Abs[Flatten[u/.C:>(Mean[measurements]+usys)]]^2]+(InverseCDF[StudentTDistribution[Length[measurements]-1],(1+Erf[1/Sqrt[2]])/2]*StandardDeviation[measurements]/Sqrt[Length[measurements]])^2]})


CalculateMeasurementHU[measurements_List,opts:OptionsPattern[]] :=
    CalculateMeasurementHU[measurements,{0},0,opts]
CalculateMeasurementHU[measurements_List,u_List,opts:OptionsPattern[]] :=
    CalculateMeasurementHU[measurements,u,0,opts]
CalculateMeasurementHU[measurements_List,u_List,usys_,opts:OptionsPattern[]] :=
    (If[ OptionValue[Analyze],
         With[ {mmean = Mean[measurements],mmeansys = Mean[measurements]+usys,mn = Length[measurements],ms = StandardDeviation[measurements],uu = Abs[Flatten[u/.C:>(Mean[measurements]+usys)]],usum = Total[Abs[Flatten[u/.C:>(Mean[measurements]+usys)]]^2]+(If[ Length[measurements]<6,
                                                                                                                                                                                                                                                                   Max[Abs[measurements-Mean[measurements]]],
                                                                                                                                                                                                                                                                   StandardDeviation[measurements]/Sqrt[Length[measurements]]
                                                                                                                                                                                                                                                               ])^2,np = Count[Mean[measurements]-measurements,_?Positive],nm = Count[Mean[measurements]-measurements,_?Negative]},
             Print@Grid[{
             {Grid[{{"\!\(\*OverscriptBox[\(x\), \(_\)]\)","\!\(\*SubscriptBox[\(u\), \(sys\)]\)","\!\(\*OverscriptBox[\(x\), \(_\)]\)+\!\(\*SubscriptBox[\(u\), \(sys\)]\)","u","\!\(\*SubscriptBox[\(u\), \(sys\)]\)/\!\(\*OverscriptBox[\(x\), \(_\)]\) [%]","n","\!\(\*SubscriptBox[\(n\), \(+\)]\)","\!\(\*SubscriptBox[\(n\), \(-\)]\)","\!\(\*SubscriptBox[\(n\), \(0\)]\)","|\!\(\*SubscriptBox[\(n\), \(+\)]\)-\!\(\*SubscriptBox[\(n\), \(-\)]\)|\[LessEqual]\!\(\*SqrtBox[\(n\)]\)","\[Sigma]","\!\(\*SubscriptBox[\(min\), \(i\)]\)|\!\(\*OverscriptBox[\(x\), \(_\)]\)-\!\(\*SubscriptBox[\(x\), \(i\)]\)|","\!\(\*SubscriptBox[\(max\), \(i\)]\)|\!\(\*OverscriptBox[\(x\), \(_\)]\)-\!\(\*SubscriptBox[\(x\), \(i\)]\)|"},{mmean,usys,mmeansys,Sqrt[usum],Abs[usys/mmean]*100,mn,np,nm,mn-(np+nm),Item[ToString[Abs[np-nm]]<>"\[LessEqual]"<>ToString[N[Sqrt[mn]]],Background->If[ TrueQ[Abs[np-nm]<=Sqrt[mn]],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  LightGreen,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  LightRed
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ]],ms}~Join~MinMax[Abs[mmean-measurements]]},Frame->All,Background->{None,{LightGray,White}}]},
             {Grid[Transpose[{{"uncertanity u","N[u]","u^2","uncertanty weights [%]"}}~Join~{{If[ Length[measurements]<6,
                                                                                                  Item["\!\(\*OverscriptBox[\(s\), \(_\)]\)=\!\(\*SubscriptBox[\(max\), \(i\)]\)|\!\(\*OverscriptBox[\(x\), \(_\)]\)-\!\(\*SubscriptBox[\(x\), \(i\)]\)|",Background->LightRed],
                                                                                                  "\!\(\*OverscriptBox[\(s\), \(_\)]\)=s/\!\(\*SqrtBox[\(n\)]\)"
                                                                                              ],If[ Length[measurements]<6,
                                                                                                    Max[Abs[measurements-Mean[measurements]]],
                                                                                                    ms/Sqrt[mn]
                                                                                                ],(If[ Length[measurements]<6,
                                                                                                       Max[Abs[measurements-Mean[measurements]]],
                                                                                                       ms/Sqrt[mn]
                                                                                                   ])^2,100*(If[ Length[measurements]<6,
                                                                                                                 Max[Abs[measurements-Mean[measurements]]],
                                                                                                                 ms/Sqrt[mn]
                                                                                                             ])^2/usum}}~Join~Table[{u[[i]],uu[[i]],uu[[i]]^2,100*uu[[i]]^2/usum},{i,1,Length[u]}]]~Join~{{"Sqrt-uncertanty weights [%]"}~Join~(100*SqrtWeights[{(If[ Length[measurements]<6,
                                                                                                                                                                                                                                                                      Max[Abs[measurements-Mean[measurements]]],
                                                                                                                                                                                                                                                                      ms/Sqrt[mn]
                                                                                                                                                                                                                                                                  ])^2}~Join~uu^2])},Frame->All,Background->{None,{LightGray,White,LightGray,White}}]},
             {Show[Histogram[measurements+usys,Automatic,"PDF",PlotTheme->"Scientific",ImageSize->700,FrameTicks->{{Automatic,Automatic},{Automatic,{{mmeansys,"\[Mu]"}}~Join~Flatten[Table[{{i*ms+mmeansys,ToString[i]<>"\[Sigma]"},{-i*ms+mmeansys,"-"<>ToString[i]<>"\[Sigma]"}},{i,1,5}],1]}},AspectRatio->1/3,Epilog->{Gray,Thick,InfiniteLine[{{mmeansys,0},{mmeansys,1}}],Thin}~Join~Flatten[Table[{InfiniteLine[{{-i*ms+mmeansys,0},{-i*ms+mmeansys,1}}],InfiniteLine[{{i*ms+mmeansys,0},{i*ms+mmeansys,1}}]},{i,1,5}]],PlotRange->{{-5*ms+mmeansys,5*ms+mmeansys},All}],SmoothHistogram[measurements+usys,Automatic,"PDF",PlotRange->{{-5*ms+mmeansys,5*ms+mmeansys},All},PlotStyle->Red],Plot[Evaluate@PDF[NormalDistribution[mmeansys,ms],x],{x,-5*ms+mmeansys,5*ms+mmeansys},PlotStyle->Blue,PlotRange->{{-5*ms+mmeansys,5*ms+mmeansys},All}]]}
             },Frame->All,Background->{None,{White,LightGray}}]
         ]
     ];
     Return@{Mean[measurements]+usys,Sqrt[Total[Abs[Flatten[u/.C:>(Mean[measurements]+usys)]]^2]+(If[ Length[measurements]<6,
                                                                                                      Max[Abs[measurements-Mean[measurements]]],
                                                                                                      StandardDeviation[measurements]/Sqrt[Length[measurements]]
                                                                                                  ])^2]})


ErrorMean[data_?(MatchQ[#,List[List[_?NumericQ,_?NumericQ]..]]&)] :=
    {Mean[data[[All,1]]],ErrorMean[data[[All,2]]]}
ErrorMean[data__] :=
    1/Length[Flatten[List[data]]]*Sqrt[Total[Flatten[List[data]]^2]]


ContainsExactlyOneVariable[form_,variables_] :=
    (Count[Count[{form},#,\[Infinity]]&/@variables,_?(#!=0&)]==1)
GetContainingExactlyOneVariable[form_,variables_] :=
    variables[[Flatten[Position[Count[{form},#,\[Infinity]]&/@variables,_?(#!=0&)]][[1]]]]

StripConditional[Rule[variable_,solution_]] :=
    Rule[variable,StripConditional[solution]]
StripConditional[solution_] :=
    If[ Head[solution]===ConditionalExpression,
        solution[[1]],
        solution
    ]

AdaptiveLimitGuess[data_,form_,params_,variable_] :=
    Module[ {signAssumptions,limitLeftResults,limitRightResults,leftResults,rightResults,dataMinMax,sortData,endGuesses},
        signAssumptions = (MapThread[Apply[#1,{#2,#3}]&,{#,params,ConstantArray[0,Length@params]}])&/@Tuples[{Less,Greater},Length@params];
        limitLeftResults = Quiet@Limit[form,variable->(-\[Infinity]),Assumptions->#]&/@signAssumptions;
        limitRightResults = Quiet@Limit[form,variable->\[Infinity],Assumptions->#]&/@signAssumptions;
        limitLeftResults = DeleteDuplicates[Select[Flatten[limitLeftResults/.Interval->List],Count[{#},Limit|Infinity|DirectedInfinity|ComplexInfinity|Indeterminate,\[Infinity]]==0&]];
        limitRightResults = DeleteDuplicates[Select[Flatten[limitRightResults/.Interval->List],Count[{#},Limit|Infinity|DirectedInfinity|ComplexInfinity|Indeterminate,\[Infinity]]==0&]];
        limitLeftResults = Select[limitLeftResults,ContainsExactlyOneVariable[#,params]&];
        limitRightResults = Select[limitRightResults,ContainsExactlyOneVariable[#,params]&];
        leftResults = StripConditional/@Select[Flatten[(Quiet@Solve[#==variable,GetContainingExactlyOneVariable[#,params],Reals]&)/@limitLeftResults],Head[#]=!=Solve&];
        rightResults = StripConditional/@Select[Flatten[(Quiet@Solve[#==variable,GetContainingExactlyOneVariable[#,params],Reals]&)/@limitRightResults],Head[#]=!=Solve&];
        sortData = SortBy[data,#[[1]]&];
        dataMinMax = {sortData[[1,2]],sortData[[-1,2]]};
        endGuesses = Flatten[{leftResults/.{variable->dataMinMax[[1]]},rightResults/.{variable->dataMinMax[[2]]}}];
        Return[Function[{param},Select[endGuesses,#[[1]]==param&][[All,2]]]/@params]
    ]

AdaptiveRootGuess[data_,form_,params_,variable_] :=
    Module[ {solutions,crossings,endGuesses},
        solutions = StripConditional/@(Select[Flatten[{Quiet[Solve[form==0,variable,Reals]]}],Head[#]=!=Solve&][[All,2]]);
        solutions = Select[solutions,ContainsExactlyOneVariable[#,params]&];
        solutions = DeleteDuplicates[StripConditional/@Flatten[Quiet@Solve[#==variable,GetContainingExactlyOneVariable[#,params],Reals]&/@solutions]];
        If[ Length[solutions]==0,
            Return[ConstantArray[{},Length[params]]]
        ];
        crossings = Select[Range[Length[data]-1],Sign[data[[#,2]]]!=Sign[data[[#+1,2]]]&];
        endGuesses = Flatten[solutions/.{variable->((data[[#+1,1]]*data[[#,2]]-data[[#,1]]*data[[#+1,2]])/(data[[#,2]]-data[[#+1,2]]))}&/@crossings];
        Return[Function[{param},DeleteDuplicates@Select[endGuesses,#[[1]]==param&][[All,2]]]/@params]
    ]

AdaptivePeakGuess[data_,form_,params_,variable_] :=
    Module[ {dfunc,dsol,xdsol,xsol,ydsol,ysol,yPeaks,xPeaks,peaks,endGuesses},
        peaks = data[[Round/@DeleteCases[Flatten[(FindPeaks[(-1)^#*data[[All,2]],2*(Max[data[[All,1]]]-Min[data[[All,1]]])][[All,1]])&/@{0,1}],_?(#==1||#==Length[data]&)]]];
        If[ Length[peaks]==0,
            Return[ConstantArray[{},Length[params]]]
        ];
        xPeaks = peaks[[All,1]];
        yPeaks = peaks[[All,2]];
        dfunc = D[form,variable];
        dsol = StripConditional/@Select[Flatten[{Quiet@Solve[dfunc==0,variable,Reals]}],Head[#]=!=Solve&];
        xdsol = Select[dsol[[All,2]],ContainsExactlyOneVariable[#,params]&];
        xsol = StripConditional/@Select[Flatten[(Quiet@Solve[#==variable,GetContainingExactlyOneVariable[#,params],Reals]&)/@xdsol],Head[#]=!=Solve&];
        ydsol = Select[Flatten[{form/.dsol}],ContainsExactlyOneVariable[#,params]&];
        ysol = StripConditional/@Select[Flatten[(Quiet@Solve[#==variable,GetContainingExactlyOneVariable[#,params],Reals]&)/@ydsol],Head[#]=!=Solve&];
        endGuesses = Flatten[{(ysol/.{variable->#}&)/@yPeaks,(xsol/.{variable->#}&)/@xPeaks}];
        Return[Function[{param},DeleteDuplicates@Select[endGuesses,#[[1]]==param&][[All,2]]]/@params]
    ]

AdaptiveInverseRootGuess[data_,form_,params_,variable_] :=
    Module[ {inverse,y = Unique["y"]},
        inverse = (StripConditional/@Select[Flatten[{Quiet@Solve[form==y,variable,Reals]}][[All,2]],Head[#]=!=Solve&]);
        If[ Length[inverse]==0,
            Return[ConstantArray[{},Length[params]]]
        ];
        Return[AdaptiveRootGuess[Reverse/@data,inverse,params,y]]
    ]

AdaptiveConstantEliminate[form_,params_,variable_,guesses_] :=
    Module[ {expandedForm,constantVar,constantGuesses},
        expandedForm = Expand[form];
        If[ Head[expandedForm]=!=Plus,
            Return[{form}]
        ];
        expandedForm = Select[List@@expandedForm,FreeQ[#,variable]&];
        If[ !ContainsExactlyOneVariable[expandedForm,params],
            Return[{form}]
        ];
        constantVar = GetContainingExactlyOneVariable[expandedForm,params];
        constantGuesses = Select[guesses[[Flatten[Position[params,constantVar]][[1]]]],#!=1&];
        Return[Flatten[{form,(form/.{constantVar->#})&/@constantGuesses}]]
    ]

DeleteNumericDuplicates[list_] :=
    DeleteDuplicates[list,Abs[Abs[#1]-Abs[#2]]<10^(-6)&]

AdaptiveNonlinearModelFit[data_?(MatchQ[#,List[List[_?NumericQ,_?NumericQ]..]]&),form_,parameters_?(MatchQ[#,List[(_Symbol|List[_Symbol,_?NumericQ])..]]&),variable_Symbol,opts:OptionsPattern[]] :=
    Module[ {guesses,parameterVariables,allFits,additionalGuessList},
        guesses = If[ Head[#]===List,
                      {#[[2]]},
                      {1}
                  ]&/@parameters; (*get all starting guesses or supply them with 1*)
        parameterVariables = If[ Head[#]===List,
                                 #[[1]],
                                 #
                             ]&/@parameters; (*get all parameter variables*)
        additionalGuessList = AdaptiveLimitGuess[data,form,parameterVariables,variable];
        guesses = Join[guesses,additionalGuessList,2];
        guesses = DeleteNumericDuplicates/@guesses;
        additionalGuessList = Join@@Join[AdaptiveRootGuess[data,#,parameterVariables,variable]&/@AdaptiveConstantEliminate[form,parameterVariables,variable,guesses],{2}];
        guesses = Join[guesses,additionalGuessList,2];
        guesses = DeleteNumericDuplicates/@guesses;
        additionalGuessList = Join@@Join[AdaptivePeakGuess[data,#,parameterVariables,variable]&/@AdaptiveConstantEliminate[form,parameterVariables,variable,guesses],{2}];
        guesses = Join[guesses,additionalGuessList,2];
        guesses = DeleteNumericDuplicates/@guesses;
        additionalGuessList = Join@@Join[AdaptiveInverseRootGuess[data,#,parameterVariables,variable]&/@AdaptiveConstantEliminate[form,parameterVariables,variable,guesses],{2}];
        guesses = Join[guesses,additionalGuessList,2];
        guesses = DeleteNumericDuplicates/@guesses;
        allFits = (Quiet@NonlinearModelFit[data,form,Transpose[{parameterVariables,#}],variable,opts,MaxIterations->10000])&/@Tuples[guesses]; (*make regressions with all possible guess combinations*)
        allFits = {#,Total[#["FitResiduals"]^2]}&/@allFits; (*combine all fits with the respective sum of the residuals squared*)
        Return[MinimalBy[allFits,#[[2]]&][[1,1]]]
    (*return the fit with the least sum of squares of the fit reiduals*)
        ]


End[]


EndPackage[]
