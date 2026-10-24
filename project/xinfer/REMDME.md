XInfer
======

XInfer is a LLM inference engine written in C/C++.

It sits on top of the XOC compiler infrastructure.
The only code dependencies are:
  1. XOC project internal source (compiler?originated base infrastructure)
  2. Standard system libc / libm (open, mmap, read, write etc.)


No third-party libraries. No PyTorch. No ONNX Runtime. No OpenBLAS.
No cuBLAS. No HuggingFace transformers. No sentencepiece. No gRPC.
No protobuf. No Boost. No eigen. No BLAS. Nothing.

The only external dependency is the XOC codebase itself. Everything
else - the math kernels, the tokenizer, the JSON parser, the safetensors
loader, the mmap glue, the memory allocator, the logging, the command
line parser - is either part of XOC or written specifically for XInfer.

## Module Extension & Customization

The core design principle of XInfer is extensibility. Users can implement
custom modules by inheriting standard base interfaces to meet domain-specific
demands.



Supported models:
    gpt2            GPT-2 (124M)
    distilgpt2      DistilGPT-2
    qwen2.5_1.5B    Qwen2.5-1.5B-Instruct

Design Notes
------------

1. Temp buffer reuse
   No malloc/free happens during inference. All scratch arrays
   (hidden state, q/k/v for attention, MLP intermediates) are
   allocated once at init.

2. K/V cache
   Allocated per layer. At decode time only the new token's K/V is
   computed; historical entries are looked up directly.

3. mmap weight loading
   The safetensors file is mmap'd once. Pages are only pulled
   from disk on first access.

4. Alignment testing
   Every operator can dump its intermediate state (-dump-rt-norm,
   -dump-tensor-data, etc.) and the output is compared token-by-token
   against a PyTorch reference. This is how numerical bugs get found.


Build
-----

    ~/xoc/project/build_xinfer_release.sh


Run
---

    ~/xoc/project/xinfer.exe \
        -prompt = "Please tell me a joke!" \
        -model_name = "qwen2.5_1.5B" \
        -temperature = 1.2 \
        -topk = 40 \
        -max_seq_len = 50 \
        -cont \
        -max_tensor_data_dump_num = 0 \
        ~/model/Qwen2.5-1.5B-Instruct/vocab.json \
        ~/model/Qwen2.5-1.5B-Instruct/merges.txt \
        ~/model/Qwen2.5-1.5B-Instruct/tokenizer_config.json \
        ~/model/Qwen2.5-1.5B-Instruct/config.json \
        ~/model/Qwen2.5-1.5B-Instruct/model.safetensors


Useful Flags
------------

    -prompt = "..."          input prompt
    -model_name = "..."      model name (see supported list above)
    -temperature = 1.0       logits scaling factor
    -topk = 40               top-k sampling, 1 means greedy
    -max_seq_len = 50        max generation length
    -cont                    continuous chat mode
    -dump <file>             write debug log to file
    -dump-rt                 dump runtime intermediate state
    -dump-tensor-data        dump tensor values (slow, debug only)
    -time                    print per-stage timing


Roadmap
-------

Near term:
  - GGUF format support (interface already stubbed out)
  - SIMD-vectorized matmul (AVX2 / NEON)

Mid term:
  - More model families (Llama, other Qwen2.5 sizes)
  - Prefill optimization
  - Incremental decode tuning

## Contributing

XInfer is an open community-driven project. We welcome contributions of all
forms, including new features, module implementations, bug fixes,
documentation improvements, and benchmark optimizations.

If you contribute code to this project, you implicitly allow your code to be
distributed under the BSD-3-Clause License. Contributors retain copyright over
their own contributions. All submitted source files must preserve existing
copyright headers embedded within each individual source file.


## License
-------

BSD-3-Clause, same as the XOC compiler. Every source file carries its
own per-file copyright header. You can use, modify, and redistribute
this, commercially or otherwise.

