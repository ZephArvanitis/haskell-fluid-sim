#define X_COMPONENT 0
#define Y_COMPONENT 1
#define Z_COMPONENT 2

typedef uchar component;

// Indexed by component. Figure out how much to shift from the middle of its
// cube to get to the position of velocity component.
static float3 grid_shifts[3] = {
    (float3)(-0.5, 0.0, 0.0),
    (float3)(0.0, -0.5, 0.0),
    (float3)(0.0, 0.0, -0.5)
};

// Time step of the simulation
static float dt = 0.01; // seconds

static float cell_width = 1; // arbitrary units (cell units)


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

// Single-component velocity function
static float3 read_v(
    read_only  image3d_t v,
    int3 pos) {

    // Sampler to read from images as 3D float arrays.
    // We clamp to edge for the sake of interpolation: if we access outside the
    // boundaries, we simply want to use the values within the boundaries.
    sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE |
                        CLK_ADDRESS_CLAMP_TO_EDGE |
                        CLK_FILTER_LINEAR;

    return read_imagef(v, sampler, (float4)(pos.x, pos.y, pos.z, 0)).w;
}

// Single-component boolean image read function
static bool read_b(
    read_only  image3d_t vec,
    int3 pos) {

    // Sampler to read from images as 3D int arrays.
    // We clamp to edge for the sake of interpolation: if we access outside the
    // boundaries, we simply want to use the values within the boundaries.
    sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE |
                        CLK_ADDRESS_CLAMP_TO_EDGE |
                        CLK_FILTER_LINEAR;

    return read_imagei(vec, sampler, (int4)(pos.x, pos.y, pos.z, 0)).w;
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
    int i = get_global_id(0);
    int j = get_global_id(1);
    int k = get_global_id(2);
 
    // 1. Find location of the face center.
    float3 x = (float3)(i, j, k) + grid_shifts[comp];

 e   // 2. Get velocity at face center we're interested in.
    float3 vel = velocity(vx, vy, vz, x);
h
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

// Deal with body forces. For now, we only use/care about gravity
kernel void body_forces(
        read_only  image3d_t vz,           // Z-coordinate velocities
        write_only image3d_t new_vz        // Force-affected velocities (output).
        ) {
    
    float gravity = -9.8; // Eventually probably multiply by some scaling constant
                          // to make this sensible

    // Get position of the cube we care about: (i, j, k) is the center
    int i = get_global_id(0);
    int j = get_global_id(1);
    int k = get_global_id(2);

    // Get the z velocity
    float vel = read_v(vz, (float3)(i, j, k));

    // And write the output
    write_imagef(new_vz, (int4)(i, j, k, 0), (float4)(0, 0, 0, vel + gravity * dt));
}

// Project
kernel void project(
        int n,                             // Side length of the cube grid.
        read_only  image3d_t vx,           // X-coordinate velocities on faces (n+1 on each side)
        read_only  image3d_t vy,           // Y-coordinate velocities
        read_only  image3d_t vz,           // Z-coordinate velocities
        read_only  image3d_t is_solid,     // boolean image - 0 for non-solid, 1 for solid
        write_only image3d_t b_mem,        // The b vector, which includes divergences
        write_only image3d_t projected_v   // Advected velocities (output).
        ) {
    int i = get_global_id(0);
    int j = get_global_id(1);
    int k = get_global_id(2);

    // Compute b (divergences + modifications for boundaries ??)
    // Use finite differences for divergence
    float xminus = read_v(vx, (float3)(i, j, k));
    float yminus = read_v(vy, (float3)(i, j, k));
    float zminus = read_v(vz, (float3)(i, j, k));
    float xplus  = read_v(vx, (float3)(i + 1, j, k));
    float yplus  = read_v(vy, (float3)(i, j + 1, k));
    float zplus  = read_v(vz, (float3)(i, j, k + 1));

    float dx = xplus - xminus;
    float dy = yplus - yminus;
    float dz = zplus - zminus;

    float divergence = (dx + dy + dz) / cell_width;

    // Account for motion of solids neighboring this cell
    // Look at neighbors, see if they're solid. If they are, add a term to b
    float b = 0;
    float solid_vel = 0;
    // -x
    b += read_b(is_solid, (int3)(i - 1, j, k)) * (xminus - solid_vel) / cell_width;
    // +x
    b += read_b(is_solid, (int3)(i + 1, j, k)) * (xplus - solid_vel) / cell_width;
    // -y
    b += read_b(is_solid, (int3)(i, j - 1, k)) * (yminus - solid_vel) / cell_width;
    // +y
    b += read_b(is_solid, (int3)(i, j + 1, k)) * (yplus - solid_vel) / cell_width;
    // -z
    b += read_b(is_solid, (int3)(i, j, k - 1)) * (zminus - solid_vel) / cell_width;
    // +z
    b += read_b(is_solid, (int3)(i, j, k + 1)) * (zplus - solid_vel) / cell_width;

    b -= divergence;

    write_imagef(b_mem, (int4)(i, j, k, 0), (float4)(0, 0, 0, b));

    // Other things here

}

