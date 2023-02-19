//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------

unit uTerraLoader;

(* The uTerrainLoader to draw terrains with GeoObjects *)

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Dialogs,

  GLS.Scene,
  GLS.VectorFileObjects,
  GLS.MeshUtils,
  GLS.Mesh,
  // GLOxOde - Old unit  needs to rewrite for Newton
  GLS.Material,
  GLS.Texture,
  GLS.VectorLists,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Strings,


  fMapWindow,
  uTerraModel,
  GBGeometry

  //CONTOURING
  ///PColor,
  ///Contour
  ;

procedure LoadTerrainToMesh(var SMesh: TGLMesh);

var
  mesh:     TGLMeshObject;
  i, j, k, n: integer;
  Tri:      TGeoSceneModel; //Triangle
  MatrixSize: integer;
  //theContours: TContourLines;
  //GSDepths: TArray;
  GSDepths: TMatrix2D;
  MeshTris: TGLAffineVectorList;

//========================================================================
implementation
//========================================================================

uses
  uGlobals, 
  fTerraScene,
  uTerraObjects,
  uResStrings,
  fTerraContours;

procedure LoadTerrainToMesh(var SMesh: TGLMesh);
var
  fg:  TFGVertexIndexList;
  Normals: TGLAffineVectorList;
  Indices: TGLIntegerList;
  RotVector: TVector3f;
  v:   TVector4f;
  HlShader: TGLShader;
  idc: array [1..20] of integer;
  ve:  TAffineVector;

begin
  if (fmMapWindow <> nil) then
  begin
    Tri := TGeoSceneModel.Create;
    if Tri.LoadTerrain(fmMapWindow.Model.ModelName) then
    begin
      //Loading relief (from database) in OxOdeStaMesh object
      if Tri <> nil then
      begin
        SMesh.BeginUpdate;
        //SMesh.MeshObjects.Clear;
        MeshTris := TGLAffineVectorList.Create;
        //normals:=TAffineVectorList.Create;
        indices  := TGLIntegerList.Create;

        Tri.Vertices_Centering;
        map.LoadMapToBmp(bmpScale, Tri);

        for i := 0 to Tri.TriCount - 1 do
        begin
          MeshTris.Add(Tri.VerticesArray[Tri.TrianglesArray[i].v1 - 1].X,
            Tri.VerticesArray[Tri.TrianglesArray[i].v1 - 1].Y,
            Tri.VerticesArray[Tri.TrianglesArray[i].v1 - 1].Z);

          MeshTris.Add(Tri.VerticesArray[Tri.TrianglesArray[i].v2 - 1].X,
            Tri.VerticesArray[Tri.TrianglesArray[i].v2 - 1].Y,
            Tri.VerticesArray[Tri.TrianglesArray[i].v2 - 1].Z);

          MeshTris.Add(Tri.VerticesArray[Tri.TrianglesArray[i].v3 - 1].X,
            Tri.VerticesArray[Tri.TrianglesArray[i].v3 - 1].Y,
            Tri.VerticesArray[Tri.TrianglesArray[i].v3 - 1].Z);

          Tri.TrianglesArray[i].Selected := False;
          Tri.TrianglesArray[i].No := i + 2;
        end;



        for i := 0 to Tri.TriCount - 1 do
        begin
          indices.Add(i * 3, i * 3 + 1, i * 3 + 2);
        end;


            {normals:=BuildNormals(meshTris, indices);

            for i := 0 to MeshTris.Count - 1 do
            begin
              k:=1;
              for j:=(i+1) to MeshTris.Count - 1 do
              begin
                if ((MeshTris.Items[i][0]=MeshTris.Items[j][0]) and
                    (MeshTris.Items[i][1]=MeshTris.Items[j][1]) and
                    (MeshTris.Items[i][2]=MeshTris.Items[j][2])) then
                begin
                    idc[k]:=j;
                    inc(k)
                end;
              end;
              for n := 0 to k - 1 do
                begin
                  ve[0]:=ve[0]+normals[idc[n]][0];
                  ve[1]:=ve[1]+normals[idc[n]][1];
                  ve[2]:=ve[2]+normals[idc[n]][2];
                end;


                NormalizeVector(ve);
               for n := 0 to k - 1 do
                begin
                  normals[idc[n]]:=ve;
                end;
            end;   }

        Smesh.BeginUpdate;
        // ox Smesh.MeshObjects.Clear;
        // mesh := TMeshObject.CreateOwned(SMesh.MeshObjects);

        mesh.Vertices  := MeshTris;
        mesh.TexCoords := MeshTris;
        ///mesh.Normals   := BuildNormals(meshTris, indices);
        mesh.Mode      := momFaceGroups;




        //---------------------------------------------------------------------
        RotVector.X := 0;
        RotVector.Y := 1;
        RotVector.Z := 0;

        //ROTATE MATRIX AROUND AXIS Y
        MeshTris.TransformAsVectors(CreateRotationMatrix(RotVector, pi));

        RotVector.X := 1;
        RotVector.Y := 0;
        RotVector.Z := 0;

        //ROTATE MATRIX AROUND AXIS X
        MeshTris.TransformAsVectors(CreateRotationMatrix(RotVector, -pi / 2));
        //---------------------------------------------------------------------



        for i := 0 to Tri.TriCount do
        begin
          fg      := TFGVertexIndexList.CreateOwned(mesh.FaceGroups);
          fg.Mode := fgmmTriangles;
          fg.VertexIndices.Capacity := 3;
          fg.VertexIndices.Add(3 * i, 3 * i + 1, 3 * i + 2);
          fg.MaterialName :=
            'm' + IntToStr(Tri.TrianglesArray[i].MaterialNo);
        end;


        FreeMemory(fg);
        FreeMemory(indices);
        FreeMemory(normals);

        SMesh.EndUpdate;

        TriMesh_Height := Round(Tri.Max.z);
        model_w := abs(round(Tri.Max.X)) + abs(round(Tri.Min.X));
        model_h := abs(round(Tri.Max.Y)) + abs(round(Tri.Min.Y));
      end;
    end
    else
    begin
      ShowMessage(LoadResString(@rsErrorLoadingFile) + 'dfg');
      //Cannot load terrain
      fmGeoScene.Close;
    end;

    //Smesh.MeshObjects[0].TexCoords;
    CurrentModelName := 'Geoscene - ' + Tri.TheTableName;
        { ox
        for i := 0 to Tri.Materials.Count - 1 do
        begin
            with SMesh.MaterialLibrary.AddTextureMaterial('m' +
                    IntToStr(Tri.Materials.Materials[i].No),
                    Tri.Materials.Materials[i].Image) do
            begin
                SetVector(v, 1, 1, 1, 1);
                Material.FrontProperties.Ambient.Color := v;
                Material.BackProperties.Ambient.Color := v;
            end;
        end;
        }
     { for i:=0 to SMesh.MaterialLibrary.Materials.Count do
        with SMesh.MaterialLibrary.Materials do
        begin
            Items[i].Shader:=hlShader;
        end;  }

    //Tri.FreeDataBase;
    //FreeAndNil(Tri);
  end;

  // ox SMesh.InitODE;
end;

end.
