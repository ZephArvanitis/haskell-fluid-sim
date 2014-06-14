// Single-component float image read function
static float read_f(
    read_only  image3d_t vec,
    int x, int y, int z) {

    // Sampler to read from images as 3D arrays.  We clamp to the border color.
    // In order to make the border color true or one, use CL_R; in order to
    // make the border color false or zero, use CL_A.
    // Using CLK_FILTER_LINEAR is good for interpolation but lousy for
    // indexing, so we don't want to use it here. No filtering!
    sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE |
                        CLK_ADDRESS_CLAMP |
                        CLK_FILTER_NEAREST;

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


kernel void apply_A(read_only image3d_t v,        // Vector to multiply A by
                    // Make sure that all A_* matrices are of type CL_A, because we assume that
                    // the border color is zero when accessing the matrices.
                    read_only image3d_t A_diag,   // "A" matrix diagonal entry for each cell.
                    read_only image3d_t A_xplus,  // "A" matrix entry in the positive x for each cell.
                    read_only image3d_t A_yplus,  // "A" matrix entry in the positive y for each cell.
                    read_only image3d_t A_zplus,  // "A" matrix entry in the positive z for each cell.
                    write_only image3d_t out      // Output vector: A * v
        ) {
    int i = get_global_id(0);
    int j = get_global_id(1);
    int k = get_global_id(2);

    float output = 0.0f;
    output += read_f(A_diag, i, j, k) * read_f(v, i, j, k);

    output += read_f(A_xplus, i, j, k) * read_f(v, i+1, j, k);
    output += read_f(A_xplus, i-1, j, k) * read_f(v, i-1, j, k);

    output += read_f(A_yplus, i, j, k) * read_f(v, i, j+1, k);
    output += read_f(A_yplus, i, j-1, k) * read_f(v, i, j-1, k);

    output += read_f(A_zplus, i, j, k) * read_f(v, i, j, k+1);
    output += read_f(A_zplus, i, j, k-1) * read_f(v, i, j, k-1);

    write_out(out, output);
}

kernel void elementwise_mult(read_only image3d_t v1,        // First  vector in dot product
                             read_only image3d_t v2,        // Second vector in dot product
                             write_only image3d_t out       // Output vector of pairwise products
        ) {
    int i = get_global_id(0);
    int j = get_global_id(1);
    int k = get_global_id(2);

    write_out(out, read_f(v1, i, j, k) * read_f(v2, i, j, k));
}

// Only valid for 2^n width grids. (...for integer n. Whole n, even! Also odd n.)
kernel void sum_step(
        read_only image3d_t in, 
        write_only image3d_t out
    ) {

    // Width of the grid that needs to be filled with useful values.
    int out_width = get_global_size(0);

    int i = get_global_id(0);
    int j = get_global_id(1);
    int k = get_global_id(2);

    float sum = 0;
    sum += read_f(in, i,             j,             k);
    sum += read_f(in, i + out_width, j,             k);
    sum += read_f(in, i,             j + out_width, k);
    sum += read_f(in, i,             j,             k + out_width);
    sum += read_f(in, i + out_width, j + out_width, k);
    sum += read_f(in, i + out_width, j,             k + out_width);
    sum += read_f(in, i,             j + out_width, k + out_width);
    sum += read_f(in, i + out_width, j + out_width, k + out_width);

    write_out(out, sum);
}
