#ifdef __cplusplus
extern "C" {
#endif

//in:input tensor
//w:weight tensor
//b:beta bias, optional and can be NULL.
//out:output buffer.
//num_group:total number of norm groups = total_elem/dim 
//    e.g:shape[2,3,4], total_elem=24,dim=4,num_group is 6.
//dim:hidden dim size (norm dim)
//eps:epsilon.
void rmsnorm(
    void * out, void const* in, RTTensor const* w,
    RTTensor const* b, unsigned int num_group, unsigned int dim,
    float eps);

//in_out:both input and output tensor, in-place operation.
//w:weight tensor
//b:beta bias, optional and can be NULL.
//out:output buffer.
//num_group:total number of norm groups = total_elem/dim 
//    e.g:shape[2,3,4], total_elem=24,dim=4,num_group is 6.
//dim:hidden dim size (norm dim)
//eps:epsilon.
void rmsnormInplace(
    void * in_out, RTTensor const* w,
    RTTensor const* b, unsigned int num_group, unsigned int dim,
    float eps);

#ifdef __cplusplus
}
#endif
