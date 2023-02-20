// ----------------------------------------------------------------------------
// The unit of Geoblock, http://sourceforge.net/projects/geoblock
// ---------------------------------------------------------------------------
{ ! Data browser }

unit fFileDataBrowser;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IniFiles,
  System.StrUtils,
  System.ImageList,
  Vcl.ImgList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.Buttons,

  gnuGetText,
  fInitialForm,
  cGlobals,
  uCommon,
  dBase;

type
  TfmFileDataBrowser = class(TfmInitialForm)
    PanelTop: TPanel;
    PanelMiddle: TPanel;
    PanelBottom: TPanel;
    PopupMenu: TPopupMenu;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    Rename1: TMenuItem;
    New1: TMenuItem;
    LabelPath: TLabel;
    PanelInputPath: TPanel;
    SpeedButtonBrowse: TSpeedButton;
    SpeedButtonDelete: TSpeedButton;
    ToolBarShowAs: TToolBar;
    ToolButton3: TToolButton;
    ToolButtonMap: TToolButton;
    ToolButtonTable: TToolButton;
    ToolButtonGraph: TToolButton;
    ToolButton1: TToolButton;
    tbExpand: TToolButton;
    tbCollapse: TToolButton;
    pgDatabase: TPageControl;
    tsExploring: TTabSheet;
    tsModeling: TTabSheet;
    tsReference: TTabSheet;
    TreeView: TTreeView;
    TreeView1: TTreeView;
    TreeView2: TTreeView;
    procedure TreeViewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbExpandClick(Sender: TObject);
    procedure tbCollapseClick(Sender: TObject);
  public
    procedure ReadIniFile; override;
    procedure WriteIniFile; override;
  protected
    procedure GetDirectories(Tree: TTreeView; Directory: string;
      Item: TTreeNode; IncludeFiles: Boolean);
    procedure FillFilesTreeView(const aTreeView: TTreeView; const ADir: string);
    procedure RecursionFillFilesTreeView(const aTreeView: TTreeView;
      const ADir: string; const AParentNode: TTreeNode = nil);
    procedure AddDirectories(theNode: TTreeNode; cPath: string);
  end;

var
  fmFileDataBrowser: TfmFileDataBrowser;

// ========================================================================
implementation
// ========================================================================

{$R *.dfm}
{ TfmFileDataBrowser }

procedure TfmFileDataBrowser.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  inherited;
///  GetDirectories(TreeView, DataPath, nil{ Item: TTreeNode}, True {IncludeFiles: Boolean});
  RecursionFillFilesTreeView(TreeView, DataPath);

//Localization
{
 for TreeView.AllNodes. := Low to High do
 TreeView.Items[I].Text := '_('+TreeView.Items[I].Text +')';
}
end;

procedure TfmFileDataBrowser.GetDirectories(Tree: TTreeView; Directory: string;
  Item: TTreeNode; IncludeFiles: Boolean);
var
  SearchRec: TSearchRec;
  ItemTemp: TTreeNode;
begin
  Tree.Items.BeginUpdate;
  if Directory[Length(Directory)] <> '\' then
    Directory := Directory + '\';
  if FindFirst(Directory + '*.*', faDirectory, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory = faDirectory) and
        (SearchRec.Name[1] <> '.') then
      begin
        if (SearchRec.Attr and faDirectory > 0) then
          Item := Tree.Items.AddChild(Item, SearchRec.Name);
        ItemTemp := Item.Parent;
        GetDirectories(Tree, Directory + SearchRec.Name, Item, IncludeFiles);
        Item := ItemTemp;
      end
      else if IncludeFiles then
        if SearchRec.Name[1] <> '.' then
          Tree.Items.AddChild(Item, SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  Tree.Items.EndUpdate;
end;


procedure TfmFileDataBrowser.FillFilesTreeView(const aTreeView: TTreeView;
  const ADir: string);
var
  srFolders, srFiles: TSearchRec;
  NodeFolder: TTreeNode;
begin
  if FindFirst(ADir + '\*.*', faDirectory, srFolders) = 0 then
    try
      repeat
        if (srFolders.Attr and faDirectory <> faDirectory) or
          MatchStr(srFolders.Name, ['.', '..']) // uses StrUtils
        then
          Continue;
        NodeFolder := aTreeView.Items.Add(nil, srFolders.Name);
        if FindFirst(ADir + '\' + srFolders.Name + '\*.*', faAnyFile, srFiles) = 0
        then
          try
            repeat
              if srFiles.Attr and faDirectory = faDirectory then
                Continue;
              aTreeView.Items.AddChild(NodeFolder, srFiles.Name);
            until FindNext(srFiles) > 0;
          finally
            FindClose(srFiles);
          end;
      until FindNext(srFolders) > 0;
    finally
      FindClose(srFolders);
    end;
end;

procedure TfmFileDataBrowser.RecursionFillFilesTreeView(const aTreeView
  : TTreeView; const ADir: string; const AParentNode: TTreeNode = nil);
var
  sr: TSearchRec;
  Node: TTreeNode;
begin
  if FindFirst(ADir + '\*.*', faAnyFile, sr) = 0 then
    try
      repeat
        if MatchStr(sr.Name, ['.', '..']) then // uses StrUtils
          Continue;
        Node := aTreeView.Items.AddChild(AParentNode, sr.Name);
        if sr.Attr and faDirectory = faDirectory then
          RecursionFillFilesTreeView(aTreeView, ADir + '\' + sr.Name, Node);
      until FindNext(sr) > 0;
    finally
      FindClose(sr);
    end;
end;

// --------------------------------------------------------------------
{ If it's necessary
  to populate the first TTreeView(DirTree) with all the directories
  to populate the second TTreeview(FileTree) with the files on the directory selected on DirTree
  to set icons for each folder (only folders) on DirTree
}
// Code to populate the DirTree
{
  procedure TfmFileDataBrowser.FormClick(Sender: TObject);
  var
  sr: TSearchRec;
  FileAttrs: Integer;
  theRootNode : tTreeNode;
  theNode : tTreeNode;
  begin
    FileAttrs := faDirectory;     // Only care about directories
    theRootNode := DirTree.Items.AddFirst(nil,'c:\');
    if FindFirst('c:\*.*', FileAttrs, sr) = 0 then
    begin
    repeat
    if (sr.Attr and FileAttrs) = sr.Attr then
    begin
      theNode := DirTree.Items.AddChild(theRootNode,sr.name);
      AddDirectories(theNode,'c:\'+sr.Name);
    end;
    until FindNext(sr) <> 0;
    FindClose(sr);
    end;
  //    DirTree.FullExpand;
  end;
}

// --------------------------------------------------------------------
// Code to populate the FileTree
{
procedure TfmFileDataBrowser.FilteredTV(theDir: string; ext: String; startNode: TTreeNode);
var
  sr: TSearchRec;
  FileAttrs: integer;
  theNode: TTreeNode;
begin
  if copy(ext, 1, 1) <> '.' then
    ext := '.' + ext;
  FileAttrs := faAnyFile;
  if startNode = nil then
    startNode := FileTree.Items.AddFirst(nil, theDir);
  if FindFirst(theDir + '\*.*', FileAttrs, sr) = 0 then
  begin
    repeat
      if (sr.Attr = faDirectory) and (copy(sr.Name, 1, 1) <> '.') then
      begin
        theNode := FileTree.Items.AddChild(startNode, sr.Name);
        theNode.ImageIndex := 0; // Use folder image for directories
        FilteredTV(theDir + '\' + sr.Name, ext, theNode);
      end
      else if ((sr.Attr and FileAttrs) = sr.Attr) and
        (ExtractFileExt(sr.Name) = ext) then
      begin
        theNode := FileTree.Items.AddChild(startNode, sr.Name);
        theNode.ImageIndex := -1; // No image for files
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
  FileTree.FullExpand;
end;
}

// ---------------------------------------------------------------------------

procedure TfmFileDataBrowser.AddDirectories(theNode: TTreeNode; cPath: string);
var
  sr: TSearchRec;
  FileAttrs: integer;
  theNewNode: TTreeNode;
begin
{
  FileAttrs := faDirectory; // Only care about directories
  if FindFirst(cPath + '\*.*', FileAttrs, sr) = 0 then
  begin
    repeat
      if ((sr.Attr and FileAttrs) = sr.Attr) and (copy(sr.Name, 1, 1) <> '.')
      then
      begin
        theNewNode := DirTree.Items.AddChild(theNode, sr.Name);
        AddDirectories(theNewNode, cPath + '\' + sr.Name);
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
}
end;

{
  We need to add an image list to form, add a folder icon to it
  (there is one in the borland common files) and then associated
  the image list with the directory treeview and the filetree treeview

  EXAMPLE OF HOW TO CALL  procedure FilteredTV
  Attach the following code to the OnClick event of the directory tree
}

// -------------------------------
{
procedure TfmFileDataBrowser.DirTreeClick(Sender: TObject);
var
  cBuild: string;
  theNode: TTreeNode;
begin
  if DirTree.Selected <> nil then
  begin
    theNode := DirTree.Selected;
    cBuild := theNode.Text;
    while theNode.Parent <> nil do
    begin
      cBuild := theNode.Parent.Text + '\' + cBuild;
      theNode := theNode.Parent;
    end;
    cBuild := stringReplace(cBuild, '\\', '\', [rfReplaceAll]);
    FilteredTV(cBuild, 'pdf', nil); // or *.db, etc. *.*
  end;
end;
}

{
procedure TfmFileDataBrowser.FillFilesTreeView(const aTreeView: TTreeView;
  const ADir: string);
var
  srFolders, srFiles: TSearchRec;
  NodeFolder: TTreeNode;
begin
  if FindFirst(ADir + '\*.*', faDirectory, srFolders) = 0 then
    try
      repeat
        if (srFolders.Attr and faDirectory <> faDirectory) or
          MatchStr(srFolders.Name, ['.', '..']) // uses StrUtils
        then
          Continue;
        NodeFolder := aTreeView.Items.Add(nil, srFolders.Name);
        if FindFirst(ADir + '\' + srFolders.Name + '\*.*', faAnyFile, srFiles) = 0
        then
          try
            repeat
              if srFiles.Attr and faDirectory = faDirectory then
                Continue;
              aTreeView.Items.AddChild(NodeFolder, srFiles.Name);
            until FindNext(srFiles) > 0;
          finally
            FindClose(srFiles);
          end;
      until FindNext(srFolders) > 0;
    finally
      FindClose(srFolders);
    end;
end;
}

// -------------------------------------------------------------------

procedure TfmFileDataBrowser.tbExpandClick(Sender: TObject);
begin
  inherited;
  TreeView.FullExpand;
end;

procedure TfmFileDataBrowser.tbCollapseClick(Sender: TObject);
begin
  inherited;
  TreeView.FullCollapse;
end;

procedure TfmFileDataBrowser.TreeViewClick(Sender: TObject);
begin
  inherited;
  //
end;

procedure TfmFileDataBrowser.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      ToolButtonMap.Down := ReadBool(Name, ToolButtonMap.Name, True);
      if ToolButtonMap.Down then
        ToolButtonMap.Click;
      ToolButtonTable.Down := ReadBool(Name, ToolButtonTable.Name, False);
      if ToolButtonTable.Down then
        ToolButtonTable.Click;
      ToolButtonGraph.Down := ReadBool(Name, ToolButtonGraph.Name, False);
      if ToolButtonGraph.Down then
        ToolButtonGraph.Click;

    finally
      IniFile.Free;
    end;
end;

procedure TfmFileDataBrowser.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteBool(Name, ToolButtonMap.Name, ToolButtonMap.Down);
      WriteBool(Name, ToolButtonTable.Name, ToolButtonTable.Down);
      WriteBool(Name, ToolButtonGraph.Name, ToolButtonGraph.Down);
    finally
      IniFile.Free;
    end;
end;

end.
