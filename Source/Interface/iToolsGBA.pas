 //------------------------------------------------------------------------------
 // The modeling system Geoblock http://sourceforge.net/projects/geoblock
 //------------------------------------------------------------------------------
 { The interfaces - TODO}
 { GBA Prefix - Geo Block API}

unit iToolsGBA;

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  Vcl.Menus,
  Vcl.ActnList,
  Vcl.Graphics,
  Vcl.ImgList,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls;


{ NTA Prefix - Native Tools API
  Access these interfaces requires that the user compile requiring vclxx.bpl
  since actual objects are passed among the interfaces. }

const
  // The following string constants are the internal names of editor macro
  // "scripts" that get executed in response to certain menu commands.  This
  // allows a menu command to execute the exact same internal "macro script"
  // as the direct key binding.

  mcGetFindString = 'GetFindString';
  mcReplace = 'Replace';
  mcRepeatSearch = 'RepeatSearch';
  mcIncrementalSearch = 'IncrementalSearch';
  mcGotoLine = 'GotoLine';
  mcClipCut = 'ClipCut';
  mcClipCopy = 'ClipCopy';
  mcClipPaste = 'ClipPaste';
  mcClipClear = 'ClipClear';
  mcHelpKeywordSearch = 'HelpKeywordSearch';
  mcOpenFileAtCursor = 'OpenFileAtCursor';
  mcToggleBreakpoint = 'ToggleBreakpoint';
  mcUndo   = 'Undo';
  mcRedo   = 'Redo';
  mcModify = 'Modify';

  { Default IDE application/project types }
  sBatfile  = 'Batfile';
  sDatabase = 'Database';
  sModel    = 'Model';
  sPlugin   = 'Plugin';

  { Toolbar names }
  sMainToolBar     = 'MainToolBar';
  sStandardToolBar = 'StandardToolBar';
  sEditToolBar     = 'EditToolBar';
  sViewToolBar     = 'ViewToolBar';
  sToolsToolBar    = 'ToolsToolBar';

  { Plugin can links to:
    Menu
    Problem Book - by default
    Interpolation Methods
    Inclination Methods
    Import Wizards
    Export Wizards
  }

const
  { Default Windows types }
  sForm   = 'Form'; // Table with a form editor
  sText   = 'Text'; // Raw single file
  sMap    = 'Map'; // 2D and 3D map window
  sGraph  = 'Graph'; // Graphics, window
  sTable  = 'Table'; // Tables window
  sReport = 'Report'; // Report window

type
  { Available option name expressed as a name and a type }
  TGBAOptionName = record
    Name: string;
    Kind: TTypeKind;
  end;

  TGBAModuleType = type integer;
  { mtProjectGroup
    mtProject
      mtTables
        mtDataBase
          mtHydrogeology
          mtGeneralBase
          mtExploration
          mtSurveying
          mtDressing
          mtMining
        mtDataSpace
          mtPoints
            mtPoints2D
            mtPoints3D
          mtDhole
          mtTin
          mtGrid
            mtGrid2D
            mtGrid3D
          mtPolygons
          mtSolid
          mtMesh
            mtMesh2D
            mtMesh3D
      mtText
      mtPlugin
  }

  { Dynamic array of option names }
  TGBAOptionNameArray = array of TGBAOptionName;

  IGBAEditor     = interface;
  IGBACreator    = interface;
  IGBAModule     = interface;
  IGBAModuleInfo = interface;
  IGBAModuleNotifier = interface;
  IGBANotifier   = interface;
  IGBAOptions    = interface;
  IGBAProject    = interface;
  IGBAProjectGroup = interface;
  IGBAProjectGroupCreator = interface;
  IGBAProjectOptions = interface;
  IGeoblockAPIServices = interface;

  TNotifyType = (ntAfterSave, ntBeforeSave, ntDestroyed, ntModified);

  IGBAEditor = interface(IUnknown)
    ['{D14CB1DA-5CF5-482F-A308-6EB865884CF2}']
    { Call this to register an IGBANotifier. The result is the index to be
      used when calling RemoveNotifier. If <0 then an error occured. }
    function AddNotifier(const ANotifier: IGBANotifier): integer;
    { Returns the actual filename of this module editor. Rename through
      IGBAModule}
    function GetFileName: string;
    { Returns the editor specific modified status }
    function GetModified: boolean;
    { Returns the associated IGBAModule }
    function GetModule: IGBAModule;
    { Mark this editor modified.  The associated module will also be modified }
    function MarkModified: boolean;
    { Call with the index obtained from AddNotifier }
    procedure RemoveNotifier(Index: integer);
    { Show this editor.  If no views are active, at least one will be created }
    procedure Show;

    property FileName: string Read GetFileName;
    property Modified: boolean Read GetModified;
    property Module: IGBAModule Read GetModule;
  end;

  IGBANotifier = interface(IUnknown)
    ['{30DBC17E-709C-4C05-B670-FC5395363978}']
    { This procedure is called immediately after the item is successfully saved.
      This is not called for IGBAPlugin }
    procedure AfterSave;
    { This function is called immediately before the item is saved. This is not
      called for IGBAPlugin }
    procedure BeforeSave;
    { The associated item is being destroyed so all references should be dropped.
      Exceptions are ignored. }
    procedure Destroyed;
    { This associated item was modified in some way. This is not called for
      IGBAPlugin }
    procedure Modified;
  end;

  IGBAModuleNotifier = interface(IGBANotifier)
    ['{56CE0FB5-2C1C-42BB-8CBB-5922CF0B096A}']
    { CheckOverwrite is called during a SaveAs operation to determine if any
      files associated with this module will overwrite any other files.
      Return True to allow the overwrite or no overwrite will occur }
    function CheckOverwrite: boolean;
    { User has renamed the module }
    procedure ModuleRenamed(const NewName: string);
  end;

  IGBAModuleInfo = interface(IUnknown)
    ['{2472C1B9-5B42-4CAF-AB20-814E139D2355}']
    { Returns the type of this module }
    function GetModuleType: TGBAModuleType;
    { Returns the Module Name }
    function GetName: string;
    { Returns the Module File name }
    function GetFileName: string;
    { Opens and returns the IOTAModule associated with this IOTAModuleInfo }
    function OpenModule: IGBAModule;

    property ModuleType: TGBAModuleType Read GetModuleType;
    property Name: string Read GetName;
    property FileName: string Read GetFileName;
  end;

  IGBAModule = interface(IUnknown)
    ['{543E6620-F37C-4762-93AA-9DDFFED94409}']
    { Call this to register an IOTANotifier. The result is the index to be
      used when calling RemoveNotifier. If <0 then an error occured. }
    function AddNotifier(const ANotifier: IGBAModuleNotifier): integer;
    { Attempt to close this module. True was successful and all references to
      this module must be released. False if this module was not closed. }
    function Close: boolean;
    { Return the filename associated with this module.  This is only the base
      name used by the IDE.  Header source and Vcl.Forms are obtained other ways.}
    function GetFileName: string;
    { Returns the number of associated files (eg. Unit1.Pas and Unit1.dfm) }
    function GetModuleFileCount: integer;
    { Return the number of open projects than own this module }
    function GetOwnerCount: integer;
    { Return the Indexed Project that owns this module }
    function GetOwner(Index: integer): IGBAProject;
    { Call with the index obtained from AddNotifier }
    procedure RemoveNotifier(Index: integer);
    { Save the module. ChangeName invokes the SaveAs logic.  ForceSave will not
      ask to save if the module is modified. Returns False if canceled
      or an error }
    function Save(ChangeName, ForceSave: boolean): boolean;
    { Sets the module filename.  Header source and Vcl.Forms will use the base
      filename. }
    procedure SetFileName(const AFileName: string);
    { CloseModule allows an add-in to force a module closed regardless of
      whether or not it is modified.  If ForceClosed is False, then calling
      this method has the same behaviour as Close as implemented in
      IOTAModule40 }
    function CloseModule(ForceClosed: boolean): boolean;
    procedure Notify(NotifyType: TNotifyType);
    property OwnerCount: integer Read GetOwnerCount;
    property Owners[Index: integer]: IGBAProject Read GetOwner;
    property FileName: string Read GetFileName Write SetFileName;
  end;

  IGBAProject = interface(IGBAModule)
    ['{2CFA9A44-CA45-470F-930B-09262B3CB426}']
    { Return the number of owned modules }
    function GetModuleCount: integer;
    { Return the Indexed owned Module Info }
    function GetModule(Index: integer): IGBAModuleInfo;
    { Return the Project options }
    function GetProjectOptions: IGBAProjectOptions;
    { Call this function to add an arbitrary file to the project.  NOTE: some
      files have special meaning to different projects.  For example: adding
      VCL50.DCP will cause a new entry in a package project's "requires" list
      while it will be a raw file to any other project type.  Set IsUnitOrForm
      to true for files that are considered items that the project would
      process directly or indirectly (ie. .pas, .cpp, .rc, etc..) or can be
      opened in the code editor. For all others, including binary files
      (.res, .bpi, .dcp, etc..) set this to False. }
    procedure AddFile(const AFileName: string; ModuleType: TGBAModuleType);
    { Call this function to remove an arbitrary file from the project.  This
      must be a fully qualified filename.  See GetModule() above for info on
      obtaining this information from a Form name or unit name }
    procedure RemoveFile(const AFileName: string);
    property ProjectOptions: IGBAProjectOptions Read GetProjectOptions;
  end;

  IGBAOptions = interface(IUnknown)
    ['{E03A60D6-A474-4697-8F51-A0C681A5BB9E}']
    { Opens the options dialog }
    procedure EditOptions;
    { Get the value of the named option. }
    function GetOptionValue(const ValueName: string): variant;
    { Set the value of the named option. }
    procedure SetOptionValue(const ValueName: string; const Value: variant);
    { Get the list of available options for this option structure }
    function GetOptionNames: TGBAOptionNameArray;

    property Values[const ValueName: string]: variant
      Read GetOptionValue Write SetOptionValue;
  end;

  IGBAProjectOptions = interface(IGBAOptions)
    ['{5A18E596-00A3-40AD-9A44-71340432EDAA}']
    {Set the modified state of the project options}
    procedure SetModifiedState(State: boolean);
    {Get the modified state of the project options}
    function GetModifiedState: boolean;
    property ModifiedState: boolean Read GetModifiedState Write SetModifiedState;
  end;

  IGeoblockAPIServices = interface(IUnknown)
    ['{EA3D64B2-85D5-454C-AA03-B18449B980EB}']
  end;

  { This class serves as a stubbed implementation of the IGBANotifer interface.
    simply statically override the methods you intend to implement and redeclare
    IGBANotifier or descendent.  The most common overrides would probably be,
    Destroyed and Modified.  Some Subsystems do *not* call all methods since in
    some cases there is not such operation.}
  TNotifierObject = class(TInterfacedObject)
  public
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  end;

  IGBAProjectGroup = interface(IGBAModule)
    ['{350F2FF1-C3C9-4185-84F6-5C3A4DA39559}']
    { Invoke the Add New Project Dialog }
    procedure AddNewProject;
    { Invoke the Open New Project Dialog }
    procedure AddExistingProject;
    { Return the currently active project }
    function GetActiveProject: IGBAProject;
    { Number of Projects in this project group }
    function GetProjectCount: integer;
    { Return the Project interface }
    function GetProject(Index: integer): IGBAProject;
    { Remove the given project from the project group }
    procedure RemoveProject(const AProject: IGBAProject);
    { Set the active project }
    procedure SetActiveProject(const AProject: IGBAProject);
    property ActiveProject: IGBAProject Read GetActiveProject Write SetActiveProject;
    property ProjectCount: integer Read GetProjectCount;
    property Projects[Index: integer]: IGBAProject Read GetProject;
  end;

  IGBACreator = interface(IUnknown)
    ['{D153AAE3-B056-49BB-895B-324886CAD8B8}']
    { Return a string representing the default creator type in which to augment.
      See the definitions of sBatfile, sDatabase, sModel and
      sPlugin, etc.. above.  Return an empty string indicating that this
      creator will provide *all* information }
    function GetCreatorType: string;
    { Return False if this is a new module }
    function GetExisting: boolean;
    { Return the Owning module, if one exists (for a project module, this would
      be a project; for a project this is a project group) }
    function GetOwner: IGBAModule;
    { Return true, if this item is to be marked as un-named.  This will force the
      save as dialog to appear the first time the user saves. }
    function GetUnnamed: boolean;
    property CreatorType: string Read GetCreatorType;
    property Existing: boolean Read GetExisting;
    property Owner: IGBAModule Read GetOwner;
    property Unnamed: boolean Read GetUnnamed;
  end;

  IGBAProjectGroupCreator = interface(IGBACreator)
    ['{C5A68980-0EF8-4FBD-92DD-B666B017E851}']
    { Return the project group file name }
    function GetFileName: string;
    { Return True to show the source }
    function GetShowSource: boolean;
    { Create and return the project group source }
    //    function NewProjectGroupSource(const ProjectGroupName: string): IGBAFile;

    property FileName: string Read GetFileName;
    property ShowSource: boolean Read GetShowSource;
  end;

//=========================================================================
implementation
//=========================================================================

{ TNotifierObject }
procedure TNotifierObject.AfterSave;
begin
  ShowMessage('do nothing yet');
end;

procedure TNotifierObject.BeforeSave;
begin
  ShowMessage('do nothing yet');
end;

procedure TNotifierObject.Destroyed;
begin
  ShowMessage('do nothing yet');
end;

procedure TNotifierObject.Modified;
begin
  ShowMessage('do nothing yet');
end;

end.
