static int3 gridPosition(int id, int n) {
    int3 pos;
    pos.z = id % n;
    pos.y = ((id - pos.z) / n) % n;
    pos.x = (id - pos.z - n * pos.y) / (n^2);

    return pos;
}

static int gridIndex(int3 loc, int n) {
    return loc.z + loc.y * n + loc.x * (n^2);
}

uchar constant numVertsTable[256] = { 0, 3, 3, 6, 3, 6, 6, 9, 3, 6, 6, 9, 6, 9, 9,
    6, 3, 6, 6, 9, 6, 9, 9, 12, 6, 9, 9, 12, 9, 12, 12, 9, 3, 6, 6, 9, 6,
    9, 9, 12, 6, 9, 9, 12, 9, 12, 12, 9, 6, 9, 9, 6, 9, 12, 12, 9, 9, 12,
    12, 9, 12, 15, 15, 6, 3, 6, 6, 9, 6, 9, 9, 12, 6, 9, 9, 12, 9, 12, 12,
    9, 6, 9, 9, 12, 9, 12, 12, 15, 9, 12, 12, 15, 12, 15, 15, 12, 6, 9, 9,
    12, 9, 12, 6, 9, 9, 12, 12, 15, 12, 15, 9, 6, 9, 12, 12, 9, 12, 15, 9,
    6, 12, 15, 15, 12, 15, 6, 12, 3, 3, 6, 6, 9, 6, 9, 9, 12, 6, 9, 9, 12,
    9, 12, 12, 9, 6, 9, 9, 12, 9, 12, 12, 15, 9, 6, 12, 9, 12, 9, 15, 6, 6,
    9, 9, 12, 9, 12, 12, 15, 9, 12, 12, 15, 12, 15, 15, 12, 9, 12, 12, 9,
    12, 15, 15, 12, 12, 9, 15, 6, 15, 12, 6, 3, 6, 9, 9, 12, 9, 12, 12, 15,
    9, 12, 12, 15, 6, 9, 9, 6, 9, 12, 12, 15, 12, 15, 15, 6, 12, 9, 15, 12,
    9, 6, 12, 3, 9, 12, 12, 15, 12, 15, 9, 12, 12, 15, 15, 6, 9, 12, 6, 3,
    6, 9, 9, 6, 9, 12, 6, 3, 9, 6, 12, 3, 6, 3, 3, 0, };


kernel void numVertices(global float *grid, global int *nVerts, int n) {
    int id = get_global_id(0);
    int3 gridPos = gridPosition(id, n);

    float field[8];
    field[0] = grid[gridIndex(gridPos, n)];
    field[1] = grid[gridIndex(gridPos + (int3)(1, 0, 0), n)];
    field[2] = grid[gridIndex(gridPos + (int3)(1, 1, 0), n)];
    field[3] = grid[gridIndex(gridPos + (int3)(0, 1, 0), n)];
    field[4] = grid[gridIndex(gridPos + (int3)(0, 0, 1), n)];
    field[5] = grid[gridIndex(gridPos + (int3)(1, 0, 1), n)];
    field[6] = grid[gridIndex(gridPos + (int3)(1, 1, 1), n)];
    field[7] = grid[gridIndex(gridPos + (int3)(0, 1, 1), n)];

    uchar cubeindex = 0;
    float isolevel = 0.1;
    if (field[0] < isolevel) cubeindex |= 1;
    if (field[1] < isolevel) cubeindex |= 2;
    if (field[2] < isolevel) cubeindex |= 4;
    if (field[3] < isolevel) cubeindex |= 8;
    if (field[4] < isolevel) cubeindex |= 16;
    if (field[5] < isolevel) cubeindex |= 32;
    if (field[6] < isolevel) cubeindex |= 64;
    if (field[7] < isolevel) cubeindex |= 128;

    nVerts[id] = numVertsTable[cubeindex];
}


// triangle table maps same cube vertex index to a list of up to 5 triangles
// which are built from the interpolated edge vertices
#define X 255
uchar constant triTable[256][16] = {
    {X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{0, 8, 3, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{0, 1, 9, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{1, 8, 3, 9, 8, 1, X, X, X, X, X, X, X, X, X, X},
	{1, 2, 10, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{0, 8, 3, 1, 2, 10, X, X, X, X, X, X, X, X, X, X},
	{9, 2, 10, 0, 2, 9, X, X, X, X, X, X, X, X, X, X},
	{2, 8, 3, 2, 10, 8, 10, 9, 8, X, X, X, X, X, X, X},
	{3, 11, 2, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{0, 11, 2, 8, 11, 0, X, X, X, X, X, X, X, X, X, X},
	{1, 9, 0, 2, 3, 11, X, X, X, X, X, X, X, X, X, X},
	{1, 11, 2, 1, 9, 11, 9, 8, 11, X, X, X, X, X, X, X},
	{3, 10, 1, 11, 10, 3, X, X, X, X, X, X, X, X, X, X},
	{0, 10, 1, 0, 8, 10, 8, 11, 10, X, X, X, X, X, X, X},
	{3, 9, 0, 3, 11, 9, 11, 10, 9, X, X, X, X, X, X, X},
	{9, 8, 10, 10, 8, 11, X, X, X, X, X, X, X, X, X, X},
	{4, 7, 8, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{4, 3, 0, 7, 3, 4, X, X, X, X, X, X, X, X, X, X},
	{0, 1, 9, 8, 4, 7, X, X, X, X, X, X, X, X, X, X},
	{4, 1, 9, 4, 7, 1, 7, 3, 1, X, X, X, X, X, X, X},
	{1, 2, 10, 8, 4, 7, X, X, X, X, X, X, X, X, X, X},
	{3, 4, 7, 3, 0, 4, 1, 2, 10, X, X, X, X, X, X, X},
	{9, 2, 10, 9, 0, 2, 8, 4, 7, X, X, X, X, X, X, X},
	{2, 10, 9, 2, 9, 7, 2, 7, 3, 7, 9, 4, X, X, X, X},
	{8, 4, 7, 3, 11, 2, X, X, X, X, X, X, X, X, X, X},
	{11, 4, 7, 11, 2, 4, 2, 0, 4, X, X, X, X, X, X, X},
	{9, 0, 1, 8, 4, 7, 2, 3, 11, X, X, X, X, X, X, X},
	{4, 7, 11, 9, 4, 11, 9, 11, 2, 9, 2, 1, X, X, X, X},
	{3, 10, 1, 3, 11, 10, 7, 8, 4, X, X, X, X, X, X, X},
	{1, 11, 10, 1, 4, 11, 1, 0, 4, 7, 11, 4, X, X, X, X},
	{4, 7, 8, 9, 0, 11, 9, 11, 10, 11, 0, 3, X, X, X, X},
	{4, 7, 11, 4, 11, 9, 9, 11, 10, X, X, X, X, X, X, X},
	{9, 5, 4, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{9, 5, 4, 0, 8, 3, X, X, X, X, X, X, X, X, X, X},
	{0, 5, 4, 1, 5, 0, X, X, X, X, X, X, X, X, X, X},
	{8, 5, 4, 8, 3, 5, 3, 1, 5, X, X, X, X, X, X, X},
	{1, 2, 10, 9, 5, 4, X, X, X, X, X, X, X, X, X, X},
	{3, 0, 8, 1, 2, 10, 4, 9, 5, X, X, X, X, X, X, X},
	{5, 2, 10, 5, 4, 2, 4, 0, 2, X, X, X, X, X, X, X},
	{2, 10, 5, 3, 2, 5, 3, 5, 4, 3, 4, 8, X, X, X, X},
	{9, 5, 4, 2, 3, 11, X, X, X, X, X, X, X, X, X, X},
	{0, 11, 2, 0, 8, 11, 4, 9, 5, X, X, X, X, X, X, X},
	{0, 5, 4, 0, 1, 5, 2, 3, 11, X, X, X, X, X, X, X},
	{2, 1, 5, 2, 5, 8, 2, 8, 11, 4, 8, 5, X, X, X, X},
	{10, 3, 11, 10, 1, 3, 9, 5, 4, X, X, X, X, X, X, X},
	{4, 9, 5, 0, 8, 1, 8, 10, 1, 8, 11, 10, X, X, X, X},
	{5, 4, 0, 5, 0, 11, 5, 11, 10, 11, 0, 3, X, X, X, X},
	{5, 4, 8, 5, 8, 10, 10, 8, 11, X, X, X, X, X, X, X},
	{9, 7, 8, 5, 7, 9, X, X, X, X, X, X, X, X, X, X},
	{9, 3, 0, 9, 5, 3, 5, 7, 3, X, X, X, X, X, X, X},
	{0, 7, 8, 0, 1, 7, 1, 5, 7, X, X, X, X, X, X, X},
	{1, 5, 3, 3, 5, 7, X, X, X, X, X, X, X, X, X, X},
	{9, 7, 8, 9, 5, 7, 10, 1, 2, X, X, X, X, X, X, X},
	{10, 1, 2, 9, 5, 0, 5, 3, 0, 5, 7, 3, X, X, X, X},
	{8, 0, 2, 8, 2, 5, 8, 5, 7, 10, 5, 2, X, X, X, X},
	{2, 10, 5, 2, 5, 3, 3, 5, 7, X, X, X, X, X, X, X},
	{7, 9, 5, 7, 8, 9, 3, 11, 2, X, X, X, X, X, X, X},
	{9, 5, 7, 9, 7, 2, 9, 2, 0, 2, 7, 11, X, X, X, X},
	{2, 3, 11, 0, 1, 8, 1, 7, 8, 1, 5, 7, X, X, X, X},
	{11, 2, 1, 11, 1, 7, 7, 1, 5, X, X, X, X, X, X, X},
	{9, 5, 8, 8, 5, 7, 10, 1, 3, 10, 3, 11, X, X, X, X},
	{5, 7, 0, 5, 0, 9, 7, 11, 0, 1, 0, 10, 11, 10, 0, X},
	{11, 10, 0, 11, 0, 3, 10, 5, 0, 8, 0, 7, 5, 7, 0, X},
	{11, 10, 5, 7, 11, 5, X, X, X, X, X, X, X, X, X, X},
	{10, 6, 5, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{0, 8, 3, 5, 10, 6, X, X, X, X, X, X, X, X, X, X},
	{9, 0, 1, 5, 10, 6, X, X, X, X, X, X, X, X, X, X},
	{1, 8, 3, 1, 9, 8, 5, 10, 6, X, X, X, X, X, X, X},
	{1, 6, 5, 2, 6, 1, X, X, X, X, X, X, X, X, X, X},
	{1, 6, 5, 1, 2, 6, 3, 0, 8, X, X, X, X, X, X, X},
	{9, 6, 5, 9, 0, 6, 0, 2, 6, X, X, X, X, X, X, X},
	{5, 9, 8, 5, 8, 2, 5, 2, 6, 3, 2, 8, X, X, X, X},
	{2, 3, 11, 10, 6, 5, X, X, X, X, X, X, X, X, X, X},
	{11, 0, 8, 11, 2, 0, 10, 6, 5, X, X, X, X, X, X, X},
	{0, 1, 9, 2, 3, 11, 5, 10, 6, X, X, X, X, X, X, X},
	{5, 10, 6, 1, 9, 2, 9, 11, 2, 9, 8, 11, X, X, X, X},
	{6, 3, 11, 6, 5, 3, 5, 1, 3, X, X, X, X, X, X, X},
	{0, 8, 11, 0, 11, 5, 0, 5, 1, 5, 11, 6, X, X, X, X},
	{3, 11, 6, 0, 3, 6, 0, 6, 5, 0, 5, 9, X, X, X, X},
	{6, 5, 9, 6, 9, 11, 11, 9, 8, X, X, X, X, X, X, X},
	{5, 10, 6, 4, 7, 8, X, X, X, X, X, X, X, X, X, X},
	{4, 3, 0, 4, 7, 3, 6, 5, 10, X, X, X, X, X, X, X},
	{1, 9, 0, 5, 10, 6, 8, 4, 7, X, X, X, X, X, X, X},
	{10, 6, 5, 1, 9, 7, 1, 7, 3, 7, 9, 4, X, X, X, X},
	{6, 1, 2, 6, 5, 1, 4, 7, 8, X, X, X, X, X, X, X},
	{1, 2, 5, 5, 2, 6, 3, 0, 4, 3, 4, 7, X, X, X, X},
	{8, 4, 7, 9, 0, 5, 0, 6, 5, 0, 2, 6, X, X, X, X},
	{7, 3, 9, 7, 9, 4, 3, 2, 9, 5, 9, 6, 2, 6, 9, X},
	{3, 11, 2, 7, 8, 4, 10, 6, 5, X, X, X, X, X, X, X},
	{5, 10, 6, 4, 7, 2, 4, 2, 0, 2, 7, 11, X, X, X, X},
	{0, 1, 9, 4, 7, 8, 2, 3, 11, 5, 10, 6, X, X, X, X},
	{9, 2, 1, 9, 11, 2, 9, 4, 11, 7, 11, 4, 5, 10, 6, X},
	{8, 4, 7, 3, 11, 5, 3, 5, 1, 5, 11, 6, X, X, X, X},
	{5, 1, 11, 5, 11, 6, 1, 0, 11, 7, 11, 4, 0, 4, 11, X},
	{0, 5, 9, 0, 6, 5, 0, 3, 6, 11, 6, 3, 8, 4, 7, X},
	{6, 5, 9, 6, 9, 11, 4, 7, 9, 7, 11, 9, X, X, X, X},
	{10, 4, 9, 6, 4, 10, X, X, X, X, X, X, X, X, X, X},
	{4, 10, 6, 4, 9, 10, 0, 8, 3, X, X, X, X, X, X, X},
	{10, 0, 1, 10, 6, 0, 6, 4, 0, X, X, X, X, X, X, X},
	{8, 3, 1, 8, 1, 6, 8, 6, 4, 6, 1, 10, X, X, X, X},
	{1, 4, 9, 1, 2, 4, 2, 6, 4, X, X, X, X, X, X, X},
	{3, 0, 8, 1, 2, 9, 2, 4, 9, 2, 6, 4, X, X, X, X},
	{0, 2, 4, 4, 2, 6, X, X, X, X, X, X, X, X, X, X},
	{8, 3, 2, 8, 2, 4, 4, 2, 6, X, X, X, X, X, X, X},
	{10, 4, 9, 10, 6, 4, 11, 2, 3, X, X, X, X, X, X, X},
	{0, 8, 2, 2, 8, 11, 4, 9, 10, 4, 10, 6, X, X, X, X},
	{3, 11, 2, 0, 1, 6, 0, 6, 4, 6, 1, 10, X, X, X, X},
	{6, 4, 1, 6, 1, 10, 4, 8, 1, 2, 1, 11, 8, 11, 1, X},
	{9, 6, 4, 9, 3, 6, 9, 1, 3, 11, 6, 3, X, X, X, X},
	{8, 11, 1, 8, 1, 0, 11, 6, 1, 9, 1, 4, 6, 4, 1, X},
	{3, 11, 6, 3, 6, 0, 0, 6, 4, X, X, X, X, X, X, X},
	{6, 4, 8, 11, 6, 8, X, X, X, X, X, X, X, X, X, X},
	{7, 10, 6, 7, 8, 10, 8, 9, 10, X, X, X, X, X, X, X},
	{0, 7, 3, 0, 10, 7, 0, 9, 10, 6, 7, 10, X, X, X, X},
	{10, 6, 7, 1, 10, 7, 1, 7, 8, 1, 8, 0, X, X, X, X},
	{10, 6, 7, 10, 7, 1, 1, 7, 3, X, X, X, X, X, X, X},
	{1, 2, 6, 1, 6, 8, 1, 8, 9, 8, 6, 7, X, X, X, X},
	{2, 6, 9, 2, 9, 1, 6, 7, 9, 0, 9, 3, 7, 3, 9, X},
	{7, 8, 0, 7, 0, 6, 6, 0, 2, X, X, X, X, X, X, X},
	{7, 3, 2, 6, 7, 2, X, X, X, X, X, X, X, X, X, X},
	{2, 3, 11, 10, 6, 8, 10, 8, 9, 8, 6, 7, X, X, X, X},
	{2, 0, 7, 2, 7, 11, 0, 9, 7, 6, 7, 10, 9, 10, 7, X},
	{1, 8, 0, 1, 7, 8, 1, 10, 7, 6, 7, 10, 2, 3, 11, X},
	{11, 2, 1, 11, 1, 7, 10, 6, 1, 6, 7, 1, X, X, X, X},
	{8, 9, 6, 8, 6, 7, 9, 1, 6, 11, 6, 3, 1, 3, 6, X},
	{0, 9, 1, 11, 6, 7, X, X, X, X, X, X, X, X, X, X},
	{7, 8, 0, 7, 0, 6, 3, 11, 0, 11, 6, 0, X, X, X, X},
	{7, 11, 6, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{7, 6, 11, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{3, 0, 8, 11, 7, 6, X, X, X, X, X, X, X, X, X, X},
	{0, 1, 9, 11, 7, 6, X, X, X, X, X, X, X, X, X, X},
	{8, 1, 9, 8, 3, 1, 11, 7, 6, X, X, X, X, X, X, X},
	{10, 1, 2, 6, 11, 7, X, X, X, X, X, X, X, X, X, X},
	{1, 2, 10, 3, 0, 8, 6, 11, 7, X, X, X, X, X, X, X},
	{2, 9, 0, 2, 10, 9, 6, 11, 7, X, X, X, X, X, X, X},
	{6, 11, 7, 2, 10, 3, 10, 8, 3, 10, 9, 8, X, X, X, X},
	{7, 2, 3, 6, 2, 7, X, X, X, X, X, X, X, X, X, X},
	{7, 0, 8, 7, 6, 0, 6, 2, 0, X, X, X, X, X, X, X},
	{2, 7, 6, 2, 3, 7, 0, 1, 9, X, X, X, X, X, X, X},
	{1, 6, 2, 1, 8, 6, 1, 9, 8, 8, 7, 6, X, X, X, X},
	{10, 7, 6, 10, 1, 7, 1, 3, 7, X, X, X, X, X, X, X},
	{10, 7, 6, 1, 7, 10, 1, 8, 7, 1, 0, 8, X, X, X, X},
	{0, 3, 7, 0, 7, 10, 0, 10, 9, 6, 10, 7, X, X, X, X},
	{7, 6, 10, 7, 10, 8, 8, 10, 9, X, X, X, X, X, X, X},
	{6, 8, 4, 11, 8, 6, X, X, X, X, X, X, X, X, X, X},
	{3, 6, 11, 3, 0, 6, 0, 4, 6, X, X, X, X, X, X, X},
	{8, 6, 11, 8, 4, 6, 9, 0, 1, X, X, X, X, X, X, X},
	{9, 4, 6, 9, 6, 3, 9, 3, 1, 11, 3, 6, X, X, X, X},
	{6, 8, 4, 6, 11, 8, 2, 10, 1, X, X, X, X, X, X, X},
	{1, 2, 10, 3, 0, 11, 0, 6, 11, 0, 4, 6, X, X, X, X},
	{4, 11, 8, 4, 6, 11, 0, 2, 9, 2, 10, 9, X, X, X, X},
	{10, 9, 3, 10, 3, 2, 9, 4, 3, 11, 3, 6, 4, 6, 3, X},
	{8, 2, 3, 8, 4, 2, 4, 6, 2, X, X, X, X, X, X, X},
	{0, 4, 2, 4, 6, 2, X, X, X, X, X, X, X, X, X, X},
	{1, 9, 0, 2, 3, 4, 2, 4, 6, 4, 3, 8, X, X, X, X},
	{1, 9, 4, 1, 4, 2, 2, 4, 6, X, X, X, X, X, X, X},
	{8, 1, 3, 8, 6, 1, 8, 4, 6, 6, 10, 1, X, X, X, X},
	{10, 1, 0, 10, 0, 6, 6, 0, 4, X, X, X, X, X, X, X},
	{4, 6, 3, 4, 3, 8, 6, 10, 3, 0, 3, 9, 10, 9, 3, X},
	{10, 9, 4, 6, 10, 4, X, X, X, X, X, X, X, X, X, X},
	{4, 9, 5, 7, 6, 11, X, X, X, X, X, X, X, X, X, X},
	{0, 8, 3, 4, 9, 5, 11, 7, 6, X, X, X, X, X, X, X},
	{5, 0, 1, 5, 4, 0, 7, 6, 11, X, X, X, X, X, X, X},
	{11, 7, 6, 8, 3, 4, 3, 5, 4, 3, 1, 5, X, X, X, X},
	{9, 5, 4, 10, 1, 2, 7, 6, 11, X, X, X, X, X, X, X},
	{6, 11, 7, 1, 2, 10, 0, 8, 3, 4, 9, 5, X, X, X, X},
	{7, 6, 11, 5, 4, 10, 4, 2, 10, 4, 0, 2, X, X, X, X},
	{3, 4, 8, 3, 5, 4, 3, 2, 5, 10, 5, 2, 11, 7, 6, X},
	{7, 2, 3, 7, 6, 2, 5, 4, 9, X, X, X, X, X, X, X},
	{9, 5, 4, 0, 8, 6, 0, 6, 2, 6, 8, 7, X, X, X, X},
	{3, 6, 2, 3, 7, 6, 1, 5, 0, 5, 4, 0, X, X, X, X},
	{6, 2, 8, 6, 8, 7, 2, 1, 8, 4, 8, 5, 1, 5, 8, X},
	{9, 5, 4, 10, 1, 6, 1, 7, 6, 1, 3, 7, X, X, X, X},
	{1, 6, 10, 1, 7, 6, 1, 0, 7, 8, 7, 0, 9, 5, 4, X},
	{4, 0, 10, 4, 10, 5, 0, 3, 10, 6, 10, 7, 3, 7, 10, X},
	{7, 6, 10, 7, 10, 8, 5, 4, 10, 4, 8, 10, X, X, X, X},
	{6, 9, 5, 6, 11, 9, 11, 8, 9, X, X, X, X, X, X, X},
	{3, 6, 11, 0, 6, 3, 0, 5, 6, 0, 9, 5, X, X, X, X},
	{0, 11, 8, 0, 5, 11, 0, 1, 5, 5, 6, 11, X, X, X, X},
	{6, 11, 3, 6, 3, 5, 5, 3, 1, X, X, X, X, X, X, X},
	{1, 2, 10, 9, 5, 11, 9, 11, 8, 11, 5, 6, X, X, X, X},
	{0, 11, 3, 0, 6, 11, 0, 9, 6, 5, 6, 9, 1, 2, 10, X},
	{11, 8, 5, 11, 5, 6, 8, 0, 5, 10, 5, 2, 0, 2, 5, X},
	{6, 11, 3, 6, 3, 5, 2, 10, 3, 10, 5, 3, X, X, X, X},
	{5, 8, 9, 5, 2, 8, 5, 6, 2, 3, 8, 2, X, X, X, X},
	{9, 5, 6, 9, 6, 0, 0, 6, 2, X, X, X, X, X, X, X},
	{1, 5, 8, 1, 8, 0, 5, 6, 8, 3, 8, 2, 6, 2, 8, X},
	{1, 5, 6, 2, 1, 6, X, X, X, X, X, X, X, X, X, X},
	{1, 3, 6, 1, 6, 10, 3, 8, 6, 5, 6, 9, 8, 9, 6, X},
	{10, 1, 0, 10, 0, 6, 9, 5, 0, 5, 6, 0, X, X, X, X},
	{0, 3, 8, 5, 6, 10, X, X, X, X, X, X, X, X, X, X},
	{10, 5, 6, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{11, 5, 10, 7, 5, 11, X, X, X, X, X, X, X, X, X, X},
	{11, 5, 10, 11, 7, 5, 8, 3, 0, X, X, X, X, X, X, X},
	{5, 11, 7, 5, 10, 11, 1, 9, 0, X, X, X, X, X, X, X},
	{10, 7, 5, 10, 11, 7, 9, 8, 1, 8, 3, 1, X, X, X, X},
	{11, 1, 2, 11, 7, 1, 7, 5, 1, X, X, X, X, X, X, X},
	{0, 8, 3, 1, 2, 7, 1, 7, 5, 7, 2, 11, X, X, X, X},
	{9, 7, 5, 9, 2, 7, 9, 0, 2, 2, 11, 7, X, X, X, X},
	{7, 5, 2, 7, 2, 11, 5, 9, 2, 3, 2, 8, 9, 8, 2, X},
	{2, 5, 10, 2, 3, 5, 3, 7, 5, X, X, X, X, X, X, X},
	{8, 2, 0, 8, 5, 2, 8, 7, 5, 10, 2, 5, X, X, X, X},
	{9, 0, 1, 5, 10, 3, 5, 3, 7, 3, 10, 2, X, X, X, X},
	{9, 8, 2, 9, 2, 1, 8, 7, 2, 10, 2, 5, 7, 5, 2, X},
	{1, 3, 5, 3, 7, 5, X, X, X, X, X, X, X, X, X, X},
	{0, 8, 7, 0, 7, 1, 1, 7, 5, X, X, X, X, X, X, X},
	{9, 0, 3, 9, 3, 5, 5, 3, 7, X, X, X, X, X, X, X},
	{9, 8, 7, 5, 9, 7, X, X, X, X, X, X, X, X, X, X},
	{5, 8, 4, 5, 10, 8, 10, 11, 8, X, X, X, X, X, X, X},
	{5, 0, 4, 5, 11, 0, 5, 10, 11, 11, 3, 0, X, X, X, X},
	{0, 1, 9, 8, 4, 10, 8, 10, 11, 10, 4, 5, X, X, X, X},
	{10, 11, 4, 10, 4, 5, 11, 3, 4, 9, 4, 1, 3, 1, 4, X},
	{2, 5, 1, 2, 8, 5, 2, 11, 8, 4, 5, 8, X, X, X, X},
	{0, 4, 11, 0, 11, 3, 4, 5, 11, 2, 11, 1, 5, 1, 11, X},
	{0, 2, 5, 0, 5, 9, 2, 11, 5, 4, 5, 8, 11, 8, 5, X},
	{9, 4, 5, 2, 11, 3, X, X, X, X, X, X, X, X, X, X},
	{2, 5, 10, 3, 5, 2, 3, 4, 5, 3, 8, 4, X, X, X, X},
	{5, 10, 2, 5, 2, 4, 4, 2, 0, X, X, X, X, X, X, X},
	{3, 10, 2, 3, 5, 10, 3, 8, 5, 4, 5, 8, 0, 1, 9, X},
	{5, 10, 2, 5, 2, 4, 1, 9, 2, 9, 4, 2, X, X, X, X},
	{8, 4, 5, 8, 5, 3, 3, 5, 1, X, X, X, X, X, X, X},
	{0, 4, 5, 1, 0, 5, X, X, X, X, X, X, X, X, X, X},
	{8, 4, 5, 8, 5, 3, 9, 0, 5, 0, 3, 5, X, X, X, X},
	{9, 4, 5, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{4, 11, 7, 4, 9, 11, 9, 10, 11, X, X, X, X, X, X, X},
	{0, 8, 3, 4, 9, 7, 9, 11, 7, 9, 10, 11, X, X, X, X},
	{1, 10, 11, 1, 11, 4, 1, 4, 0, 7, 4, 11, X, X, X, X},
	{3, 1, 4, 3, 4, 8, 1, 10, 4, 7, 4, 11, 10, 11, 4, X},
	{4, 11, 7, 9, 11, 4, 9, 2, 11, 9, 1, 2, X, X, X, X},
	{9, 7, 4, 9, 11, 7, 9, 1, 11, 2, 11, 1, 0, 8, 3, X},
	{11, 7, 4, 11, 4, 2, 2, 4, 0, X, X, X, X, X, X, X},
	{11, 7, 4, 11, 4, 2, 8, 3, 4, 3, 2, 4, X, X, X, X},
	{2, 9, 10, 2, 7, 9, 2, 3, 7, 7, 4, 9, X, X, X, X},
	{9, 10, 7, 9, 7, 4, 10, 2, 7, 8, 7, 0, 2, 0, 7, X},
	{3, 7, 10, 3, 10, 2, 7, 4, 10, 1, 10, 0, 4, 0, 10, X},
	{1, 10, 2, 8, 7, 4, X, X, X, X, X, X, X, X, X, X},
	{4, 9, 1, 4, 1, 7, 7, 1, 3, X, X, X, X, X, X, X},
	{4, 9, 1, 4, 1, 7, 0, 8, 1, 8, 7, 1, X, X, X, X},
	{4, 0, 3, 7, 4, 3, X, X, X, X, X, X, X, X, X, X},
	{4, 8, 7, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{9, 10, 8, 10, 11, 8, X, X, X, X, X, X, X, X, X, X},
	{3, 0, 9, 3, 9, 11, 11, 9, 10, X, X, X, X, X, X, X},
	{0, 1, 10, 0, 10, 8, 8, 10, 11, X, X, X, X, X, X, X},
	{3, 1, 10, 11, 3, 10, X, X, X, X, X, X, X, X, X, X},
	{1, 2, 11, 1, 11, 9, 9, 11, 8, X, X, X, X, X, X, X},
	{3, 0, 9, 3, 9, 11, 1, 2, 9, 2, 11, 9, X, X, X, X},
	{0, 2, 11, 8, 0, 11, X, X, X, X, X, X, X, X, X, X},
	{3, 2, 11, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{2, 3, 8, 2, 8, 10, 10, 8, 9, X, X, X, X, X, X, X},
	{9, 10, 2, 0, 9, 2, X, X, X, X, X, X, X, X, X, X},
	{2, 3, 8, 2, 8, 10, 0, 1, 8, 1, 10, 8, X, X, X, X},
	{1, 10, 2, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{1, 3, 8, 9, 1, 8, X, X, X, X, X, X, X, X, X, X},
	{0, 9, 1, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{0, 3, 8, X, X, X, X, X, X, X, X, X, X, X, X, X},
	{X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X}
};
#undef X

// Which vertices does each edge connect?
uchar constant edgeTable[12][2] = {
    {0, 1},
    {1, 2},
    {2, 3},
    {3, 0},
    {4, 5},
    {5, 6},
    {6, 7},
    {7, 4},
    {0, 4},
    {1, 5},
    {2, 6},
    {3, 7},
};

// Get offsets for a vertex compared to some origin corner
float3 constant vertLocs[0] = {
    (float3)(0.0f, 0.0f, 0.0f),
    (float3)(1.0f, 0.0f, 0.0f),
    (float3)(1.0f, 1.0f, 0.0f),
    (float3)(0.0f, 1.0f, 0.0f),
    (float3)(0.0f, 0.0f, 1.0f),
    (float3)(1.0f, 0.0f, 1.0f),
    (float3)(1.0f, 1.0f, 1.0f),
    (float3)(0.0f, 1.0f, 1.0f),
};

// Interpolate between two vertices based on the field values at both
float3 vertexInterp(float field1, float field2, float3 v1, float3 v2, float isolevel) {
    float alpha = (isolevel - field1) / (field2 - field1);
    return alpha * v1 + (1 - alpha) * v2;
}

float3 calcNormal(float3 v1, float3 v2, float3 v3) {
    float4 vec1, vec2;
    vec1.xyz = v1 - v2;
    vec2.xyz = v1 - v3;

    return cross(vec1, vec2).xyz;

}

// n = size of grid
kernel void generateTriangles(
        int n,                     // size of the cube grid (one side)
        global float *grid,        // grid of field values
        global int *cubeID,        // which cube to operate on
        global int *triangleID,    // which triangle within the cube to operate on
        global int *triangleNums,  // global ids for the triangles
        global float3 *v1s,        // output for 1st vertex of triangle
        global float3 *v2s,        // output for 2nd vertex
        global float3 *v3s,        // output for 3rd vertex
        global float3 *faceNormals // output for normal vector of triangle
        ) {
    int id = cubeID[get_global_id(0)];
    int tri = triangleID[get_global_id(0)];
    int triangleNum = triangleNums[get_global_id(0)];

    int3 gridPos = gridPosition(id, n);

    // Recalculate which vertices this triangle needs to connect
    // 1. Find cube index (what kind of cube is it)
    // 2. Find edges of the cube intersected by the triangle from triangle table
    // 3. Find actual coordinates?
    float field[8];
    field[0] = grid[gridIndex(gridPos, n)];
    field[1] = grid[gridIndex(gridPos + (int3)(1, 0, 0), n)];
    field[2] = grid[gridIndex(gridPos + (int3)(1, 1, 0), n)];
    field[3] = grid[gridIndex(gridPos + (int3)(0, 1, 0), n)];
    field[4] = grid[gridIndex(gridPos + (int3)(0, 0, 1), n)];
    field[5] = grid[gridIndex(gridPos + (int3)(1, 0, 1), n)];
    field[6] = grid[gridIndex(gridPos + (int3)(1, 1, 1), n)];
    field[7] = grid[gridIndex(gridPos + (int3)(0, 1, 1), n)];

    uchar cubeindex = 0;
    float isolevel = 0.1;
    if (field[0] < isolevel) cubeindex |= 1;
    if (field[1] < isolevel) cubeindex |= 2;
    if (field[2] < isolevel) cubeindex |= 4;
    if (field[3] < isolevel) cubeindex |= 8;
    if (field[4] < isolevel) cubeindex |= 16;
    if (field[5] < isolevel) cubeindex |= 32;
    if (field[6] < isolevel) cubeindex |= 64;
    if (field[7] < isolevel) cubeindex |= 128;

    // Figure out the edges the triangle is on
    int e1 = triTable[cubeindex][tri * 3];
    int e2 = triTable[cubeindex][tri * 3 + 1];
    int e3 = triTable[cubeindex][tri * 3 + 2];
    float3 v1 = vertexInterp(field[edgeTable[e1][0]], 
                             field[edgeTable[e1][1]], 
                             vertLocs[edgeTable[e1][0]], 
                             vertLocs[edgeTable[e1][1]], isolevel);
    float3 v2 = vertexInterp(field[edgeTable[e2][0]], 
                             field[edgeTable[e2][1]], 
                             vertLocs[edgeTable[e2][0]], 
                             vertLocs[edgeTable[e2][1]], isolevel);
    float3 v3 = vertexInterp(field[edgeTable[e3][0]], 
                             field[edgeTable[e3][1]], 
                             vertLocs[edgeTable[e3][0]], 
                             vertLocs[edgeTable[e3][1]], isolevel);
    float3 faceNormal = calcNormal(v1, v2, v3);

    // Assign the outputs to communicate them to the outside world
    v1s[triangleNum] = v1;
    v2s[triangleNum] = v2;
    v3s[triangleNum] = v3;
    faceNormals[triangleNum] = faceNormal;
}
