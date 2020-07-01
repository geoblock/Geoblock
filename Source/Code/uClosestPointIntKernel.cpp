#include "ClosestPointInt.pas"
#include "system.h"
#include <math.h>

// Разобраться со структурами и массивами
typedef  struct{
				int ID;
				double X, Y, Z  ;     // coordinates
				double Value;
				double Value2;     // needed for cross variograms
				double Variance;     // needed for kriging routines
				double Error;     // needed for cross validation
  }TCoordinate;
  new TCoordinate TCoordinateArray[];
  struct PCoordinate *TCoordinate;
  typedef struct(
	 int ID;
	 double Distance, Value;
  }TCoordMatch;
  new TCoordMatch TCoordMatchArray[];

  typedef struct{
		int Size;
	TICoordinateArray Pattern;
  end;
  }TGridPattern;
  new TGridPattern TGridPatternArray[];

  typedef struct{
	 PCoordinate Item;
	 Pointer Next;  // из модуля system.pas
  end;
  }TCoordList;

  struct PCoordList *TCoordList;

 // Super_Block: TCoordListArray;     // superblock structure definition
 // Pattern:     TGridPatternArray;
 // Dist_match:  TCoordMatchArray;


 //вспоогательные внешние функции добавлять их в кернел или нет?????
 //Разобраться с типом функции
TCoordinateArray Get_Super_Block_Residents( PCoordList list;  int len)
{
  TCoordinate temp [0..10000-1];
  PCoordList prt;
  int i;
  i   = 0;
  prt = list;
  while (prt^.Next <> null) do
  {
  ///PАЗОБРАТЬСЯ С АДРЕСАМИ
	prt = *prt.Next;
	temp[i].id = *(*prt.item).id;
	temp[i].x = *(*prt.item).x;
	temp[i].y = *(*prt.item).y;
	temp[i].z = *(*prt.item).z;
	temp[i].Value = *(*prt.item).Value;
	i++
	};

  len = i;
  TCoordinateArray *residents = new TCoordinateArray[len];
  //SetLength(residents, len);
  for (i = 0; i<(len - 1);i++)
  }
	residents[i].id    = temp[i].id;
	residents[i].x     = temp[i].x;
	residents[i].y     = temp[i].y;
	residents[i].z     = temp[i].z;
	residents[i].Value = temp[i].Value;
  };

  return residents; //надо вернуть значение массива
};
 //разобраться с типом функции
TCoordinateArray merge_coordinate_lists( TCoordinateArray la;  int len_a; TCoordinateArray lb; int len_b)
{

  int i, n;
  TCoordinateArray *list = new TCoordinateArray[len_a + len_b];
 // SetLength(list, len_a + len_b);
  n = 0;
  for (i = 0; i< (len_a - 1); i++)
  {
	list[n].id   = la[i].id;
	list[n].x     = la[i].x;
	list[n].y     = la[i].y;
	list[n].z     = la[i].z;
	list[n].Value = la[i].Value;

	n++;
  };
  for (i = 0; i<(len_b - 1); i++)
  {
	list[n].id   = lb[i].id;
	list[n].x    = lb[i].x;
	list[n].y     = lb[i].y;
	list[n].z     = lb[i].z;
	list[n].Value = lb[i].Value;

	n++;
  };
  TCoordinateArray *la = new TCoordinateArray[0]
  //SetLength(la, 0);
  TCoordinateArray *lb = new TCoordinateArray[0]
  //SetLength(lb, 0);

  return list;  //вернуть массив
};
//процедура
void Cm_Sort_Points(TCoordMatchArray list; int size)
{
  temp: TCoordMatch;
   int i, j;

  if (size > 0)
  {
	for (j:= (size - 2); j>0; j--)
	{
	  i = j + 1;
	  temp.id = list[j].id;
	  temp.Value = list[j].Value;
	  temp.distance = list[j].distance;

	  while ((i < size) && (temp.distance > list[i].distance))
	  {
		list[i - 1].id      = list[i].id;
		list[i - 1].Value    = list[i].Value;
		list[i - 1].distance = list[i].distance;
		i++;
	  };

	  list[i - 1].id      = temp.id;
	  list[i - 1].Value   = temp.Value;
	  list[i - 1].distance = temp.distance;
	};

  };
};

int MIN(int i, j)
	{
		if (i < j) return i
		else
				   return j;
	};

kernel void ClosestPointInt(
//=====================================================================
//=============== Основная процедура шага Predictor ===================
//=====================================================================
global const int* IntParams,     // 0) Вектор целочисленных параметров
global const float* FloatParams, // 1) Вектор вещественных параметров
global const int* BoolParams,    // 2) Вектор логических параметров
global int* HighPoints,   //число точек
global double* Xmin, Xmax, Ymin, Ymax, Zmin, Zmax,
global const short* SB_NUM_BLOCK = 30)

{
unsigned int i = get_global_id(0);
// Индекс i последнего полезного элемента
 unsigned int MaxI = IntParams[0];
 if ( i > MaxI ) return;
//.....................................................................
TCoordinateArray Points[Highpoints]; //переданный масив из хоста
//procedure GetMinMaxXYZ
int j
  Xmin = Points[0].x;
  Xmax = xmin;
  Ymin = Points[0].y;
  Ymax = ymin;
  Zmin = Points[0].z;
  Zmax = Zmin;
  for (j = 1;  j < High(Points); j++) do
  {
	if (Xmax < Points[j].X)  Xmax = Points[j].X;
	if (Xmin > Points[j].X)  Xmin = Points[j].X;
	if (ymax < Points[j].Y)  ymax = Points[j].Y;
	if (ymin > Points[j].Y)  ymin = Points[j].Y;
	if (zmax < Points[j].Z)  zmax = Points[j].Z;
	if (zmin > Points[j].Z)  zmin := Points[j].Z;
  };

 //function Get_Sb_Nearest_Neighbors
  double X, Y, Z;
  TGridPatternArray Pattern;
  int Pattern_Len;
  TCoordListArray Super_Block;
  double radius;
  int found;
  double dist;
  //double Xmin, Ymin, Zmin
  bool Cont;
  TCoordinateArray Neighbors, cell_list;
  TCoordMatchArray Dist_match;
  double dx, dy, dz;
  int k, ii, j, len, px, py, pz, sx, sy, sz, opx, opy, opz, ofound;

  {
  Neighbors = NULL;
  Dist_match = NULL;
  opx    = -1;
  opy    = -1;
  opz    = -1;
  ofound = -1;
  px     = trunc((x - xmin) / dist);
  py     = trunc((y - ymin) / dist);
  pz     = trunc((z - zmin) / dist);

  if ((px <> opx) || (py <> opy) || (pz <> opz))
  {
	cont = True;
	found = 0;
	neighbors = NULL;
	k = 0;
	do
	{
	  if (found > Count)  cont = False;

	  for (j = 0 ; j<(pattern[k].size - 1); j++)
	  {
		sx = px + pattern[k].pattern[j].x;
		sy = py + pattern[k].pattern[j].y;
		sz = pz + pattern[k].pattern[j].z;
		if ((sx >= 0) && (sx < SB_NUM_BLOCK) && (sy >= 0) &&
		  (sy < SB_NUM_BLOCK) && (sz >= 0) && (sz < SB_NUM_BLOCK))
		{
		//разобраться с параметрами функции
		  cell_list = get_super_block_residents(@super_block[sz][sy][sx], len);
		  if (len > 0)
		  {
			if (neighbors = null) { neighbors = cell_list; }
			//разобраться с функцией
			else { neighbors = merge_coordinate_lists(neighbors, found, cell_list, len); }
			found = found + len;
		  }
		}
	  }
	  k++;
	}while !(cont && (i < pattern_len));
  }
  else { found = ofound;}

   TCoordMatchArray *dist_match = new TCoordMatchArray[found];
  //SetLength(dist_match, found);
  for (ii = 0; ii<(found - 1); ii++)
  {
	dx = neighbors[ii].x - x;
	dy = neighbors[ii].y - y;
	dz = neighbors[ii].z - z;

	dist_match[ii].id       = neighbors[ii].id;
	dist_match[ii].Value    = neighbors[ii].Value;
	dist_match[ii].distance = (dx * dx) + (dy * dy) + (dz * dz);
   }

  cm_sort_points(dist_match, found);

  for (ii := 0 ; ii< (MIN(found, Count) - 1);ii++)
  {
  dist_match[ii].distance = sqrt(dist_match[ii].distance);
   }
  opx    = px;
  opy    = py;
  opz    = pz;
  ofound = found;

  //return  dist_match;

 }
}
