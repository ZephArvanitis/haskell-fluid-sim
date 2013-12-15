int3 gridPosition(int id, int n) {
    int z = id % n;
    int y = ((id - z)/n) % n;
    int x = ((id - z - n*y) / (n^2));

    return int3(x, y, z);
}

int index(int3 loc, int n) {
    return loc.z + loc.y * n + loc.x * (n^2);
}

// number of vertices for each case above
uchar numVertsTable[256] = { 0, 3, 3, 6, 3, 6, 6, 9, 3, 6, 6, 9, 6, 9, 9,
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

__kernel void cubes(__global float *grid, __global float *nVerts, int n){
    int id = get_global_id(0);
    // get x, y, z out of id
    int3 gridPos = gridPosition(id, n);

    // read field values at neighbouring grid vertices
    float field[8];
    field[0] = grid[index(gridPos, n)];
    field[1] = grid[index(gridPos + (int3)(1, 0, 0)];
    field[2] = grid[index(gridPos + (int3)(1, 1, 0)];
    field[3] = grid[index(gridPos + (int3)(0, 1, 0)];
    field[4] = grid[index(gridPos + (int3)(0, 0, 1)];
    field[5] = grid[index(gridPos + (int3)(1, 0, 1)];
    field[6] = grid[index(gridPos + (int3)(1, 1, 1)];
    field[7] = grid[index(gridPos + (int3)(0, 1, 1)];

    uchar cubeindex = 0;
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
