Clear[ShowGUI];
ShowGUI[] := 
Module[{newFileList, DoInstall},
	DoInstall[] := Module[{}, 
		Map[
			(
				CopyFile[#, FileNameJoin[{$UserBaseDirectory, "Applications", #}], OverwriteTarget->True];
				Print["Copied ", #, " to ", FileNameJoin[{$UserBaseDirectory, "Applications", #}], "\n"]
			)&,
			newFileList
		]
	];
	Print["Current Wolfram Mathematica Character Encoding: ", $CharacterEncoding, " ; Is it \"UTF-8\"? Otherwise please note that you have to open this package with \"UTF-8\"-encoding!!!"];
	SetDirectory[StringJoin[ParentDirectory@NotebookDirectory[], "\source"]];
	newFileList = FileNames["*.m"];
	Print["These are the files to be copied: ", newFileList, ", which are inside of ", Directory[]];
	Row[{Button["Install!", DoInstall[]], Button["Clear output", NotebookDelete[Cells[EvaluationNotebook[], GeneratedCell -> True]]  ]}]
]
ShowGUI[]
