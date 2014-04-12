// Single-component float image read function
static float read_f(
    read_only  image3d_t vec,
    int x, int y, int z) {

    // Sampler to read from images as 3D bool arrays.
    // We clamp to the border color. In order to make the border color true, use CL_R;
    // in order to make the border color false, use CL_A.
    sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE |
                        CLK_ADDRESS_CLAMP |
                        CLK_FILTER_LINEAR;

    return read_imagef(vec, sampler, (int4)(x, y, z, 0)).w;
}

static void write_out(
    write_only  image3d_t img,
    float value) {

    int i = get_global_id(0);
    int j = get_global_id(1);
    int k = get_global_id(2);

    write_imagef(img, (int4)(i, j, k, 0), (float4)(0, 0, 0, value));
}

kernel void add_vec(read_only image3d_t v1,     // First vector
                    read_only image3d_t v2,     // Second vector, same length as first
                    float scale,                // Scale to apply to v2
                    write_only image3d_t out    // Output vector: v1 + (scale * v2)
        ) {
    int i = get_global_id(0);
    int j = get_global_id(1);
    int k = get_global_id(2);

    float v1_comp = read_f(v1, i, j, k);
    float v2_comp = read_f(v2, i, j, k);

    write_out(out, v1_comp + scale * v2_comp);
}

