VIIRF (Versatile IIR Filter)
This provides VHDL code and a Python configuration script for a versatile IIR filter hardware implementation. 
Hence, the VIIRF can realize any transfer function (highpass, lowpass etc.) that can be represented as a cascade of second-order sections (SOS). 
Common tools like Matlab or SciPy can be used to generate the necessary SOS data of the desired filter. 

The hardware core can provide pipelining (or not, depending on your needs). 

The configuration script takes the (floating-point) SOS (and gain-matrix G, if required) coefficients and configures the (quantized) filter. It also simulates a step-response and generates testbench-files. 

Have fun!

