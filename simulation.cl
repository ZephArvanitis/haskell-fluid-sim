#define X_COMPONENT 0
#define Y_COMPONENT 1
#define Z_COMPONENT 2

/* OLD interpolation
 *
    switch (comp) {
        case X_COMPONENT:
            vel.x = GRID_ACCESS(vx, i, j, k);
            vel.y = 0.25 *  (GRID_ACCESS(vy, i,   j,   k)
                           + GRID_ACCESS(vy, i,   j+1, k)
                           + GRID_ACCESS(vy, i-1, j,   k)
                           + GRID_ACCESS(vy, i-1, j+1, k));
            vel.z = 0.25 *  (GRID_ACCESS(vz, i,   j,   k)
                           + GRID_ACCESS(vz, i,   j,   k+1)
                           + GRID_ACCESS(vz, i-1, j,   k)
                           + GRID_ACCESS(vz, i-1, j,   k+1));
            break;
        case Y_COMPONENT:
            vel.x = 0.25 *  (GRID_ACCESS(vx, i,   j,   k)
                           + GRID_ACCESS(vx, i+1, j,   k)
                           + GRID_ACCESS(vx, i,   j-1, k)
                           + GRID_ACCESS(vx, i+1, j-1, k));
            vel.y = GRID_ACCESS(vy, i, j, k);
            vel.z = 0.25 *  (GRID_ACCESS(vz, i,   j,   k)
                           + GRID_ACCESS(vz, i,   j,   k+1)
                           + GRID_ACCESS(vz, i,   j-1, k)
                           + GRID_ACCESS(vz, i,   j-1, k+1));
            break;
        case Z_COMPONENT:
            vel.x = 0.25 *  (GRID_ACCESS(vx, i,   j, k)
                           + GRID_ACCESS(vx, i+1, j, k)
                           + GRID_ACCESS(vx, i,   j, k-1)
                           + GRID_ACCESS(vx, i+1, j, k-1));
            vel.y = 0.25 *  (GRID_ACCESS(vy, i, j,   k)
                           + GRID_ACCESS(vy, i, j+1, k)
                           + GRID_ACCESS(vy, i, j,   k-1)
                           + GRID_ACCESS(vy, i, j+1, k-1));
            vel.z = GRID_ACCESS(vz, i, j, k);
            break;
    }

// Assume that image type is CL_A
#define GRID_ACCESS(v, i, j, k) (read_imagef(v, sampler, (float4)(i, j, k, 0)).w)
 */

// Indexed by component. Figure out how much to shift from the middle of its
// cube to get to the position of velocity component.
static float3 grid_shifts[3] = {
    (float3)(-0.5, 0.0, 0.0),
    (float3)(0.0, -0.5, 0.0),
    (float3)(0.0, 0.0, -0.5)
};

typedef uchar component;


/*** Grid location and index conversion functions ***/

static float3 velocity(
    read_only  image3d_t vx,
    read_only  image3d_t vy,
    read_only  image3d_t vz,
    float3 pos) {

    // Sampler to read from images as 3D float arrays.
    // We clamp to edge for the sake of interpolation: if we access outside the
    // boundaries, we simply want to use the values within the boundaries.
    sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE |
                        CLK_ADDRESS_CLAMP_TO_EDGE |
                        CLK_FILTER_LINEAR;

    float3 vel;
    vel.x = read_imagef(v, sampler, (float4)(pos.x + 0.5, pos.y, pos.z, 0)).w;
    vel.y = read_imagef(v, sampler, (float4)(pos.x, pos.y + 0.5, pos.z, 0)).w;
    vel.z = read_imagef(v, sampler, (float4)(pos.x, pos.y, pos.z + 0.5, 0)).w;
    return vel;
}

/*** Kernels ***/

// Semi-Lagrangian advection step of the simulation.
kernel void advect(
        int n,                             // Side length of the cube grid.
        component comp,                    // Which component to advect.
        read_only  image3d_t vx,           // X-coordinate velocities on faces (n+1 on each side)
        read_only  image3d_t vy,           // Y-coordinate velocities
        read_only  image3d_t vz,           // Z-coordinate velocities
        write_only image3d_t advected      // Advected velocities (output).
        ) {
    // (i, j, k) is the center of cube we're looking at.
    // Using GRID_ACCESS gives us the lower-indexed faces of this cube.
    int i = get_global_id(0);
    int j = get_global_id(1);
    int k = get_global_id(2);

    // 1. Find location of the face center.
    float3 x = (float3)(i, j, k) + grid_shifts[comp];

    // 2. Get velocity at face center we're interested in.
    float3 vel = velocity(vx, vy, vz, x);

    // 3. Find x_mid (middle point for RK2).
    float3 x_mid = x - 0.5 * dt * vel;

    // 4. Evaluate velocity at x_mid via interpolation.
    float3 vel_mid = velocity(vx, vy, vz, x_mid);

    // 5. Find x_prev (point at previous time step).
    float3 x_prev = x - dt * vel_mid;

    // 6. Evaluate velocity component at x_prev.
    float3 vel_prev = velocity(vx, vy, vz, x_prev);

    float out;
    switch (comp) {
        case X_COMPONENT: out = vel_prev.x;
            break;
        case Y_COMPONENT: out = vel_prev.y;
            break;
        case Z_COMPONENT: out = vel_prev.z;
            break;
    }

    write_imagef(advected, (int4)(i, j, k, 0), (float4)(0, 0, 0, out));
}
