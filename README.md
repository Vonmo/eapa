# eapa
Erlang/Elixir Arbitrary Precision Arithmetic

**release:**
`make up && make rel` or `rebar3 as prod release`

**test:**
`make tests` or `rebar3 as test ct`


**rebar deps for external projects:**
```erlang
{deps, [
  {eapa, "", {git, "https://github.com/Vonmo/eapa.git", {tag, "v0.1.2"}}}
]}.  
```
**mix deps (Elixir):**
```erlang
defp deps do
 [
   {:eapa, git: "https://github.com/Vonmo/eapa.git", tag: "v0.1.2"}      
 ]
end
```
## Introduction

Most of modern microprocessors are made with a hardware implementation of real number representation in IEEE754. The number length is limited by the format, and rounding modes influence precision. Programmers are often unable to change the behaviour of equipment, or languages implementation. For example, the official implementation of Erlang stores float in 3 words on a 64-bit machine and in 4 words on a 32-bit one.

IEEE754 numbers are an infinite set represented as finite. That's why every operand can be misrepresented in IEEE754.

Most numbers when represented as a finite set have stable minimal relative error. For instance, it is 11,920928955078125e-6% for a float and 2,2204460492503130808472633361816e-14% for a double. Most programmers can afford to neglect such an error, though it should be mentioned that you can be caught in the same trap because the rate of an absolute error might be up to 10^31 and 10^292 for a float and a double respectively. It can be troublesome for your computing.

## EAPA
EAPA is a NIF extension written on Rust. So far, in EAPA repository there is an eapa_int interface. It is as simple as possible, as well as the most convenient. This interface is used for working with fixed-point numbers. Among its main features are:
1. No effects of IEEE754 encoding 	
1. Big numbers support 	
1. Customized precision up to 126 decimal places (current realization)
1. Autoscaling
1. Support of all main numerical operations
1. More or less complete testing, including property based one

### eapa_int interface
* `with_val/2` – conversion from a float into a fixed-point representation which can be safely used with json and xml. 
* `to_float/2` – conversion from a fixed-point number into a float with precision required.
* `to_float/1` – conversion from a fixed-point number into a float. 	
* `add/2` – sum of two numbers
* `sub/2` – difference
* `mul/2` – multiplication
* `divp/2` – division
* `min/2` – min number
* `max/2` – max number
* `eq/2` – equality
* `lt/2` – less than 	
* `lte/2` – less than or equals to
* `gt/2` – greater than
* `gte/2` – greater than or equals to

## Development sandbox
You can play with EAPA in Docker enviroment using `make`:
* `make tests` - run tests
* `make lint` - linter
* `make xref` - xref analysis
* `make prod` - generate release for target
