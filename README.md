# RPN Compiler and Interpreter (Clojure version)

A Clojure implementation of the RPN compiler and interpreter. See the original [RPN project](https://github.com/davidledwards/rpn) for documentation.

## Comments

I used the [Scala version](https://github.com/davidledwards/rpn) as the point of reference when porting to Clojure primarily because it was a bit closer to a pure functional experience when compared to the [Kotlin port](https://github.com/davidledwards/rpn-kotlin). Retrospectively, I find Scala a bit easier to read than Kotlin despite that it feels like a much heavier language.

Learning Clojure was enjoyable and brought back memories of my days experimenting with Lisp more than 25 years ago. I was a bit fascinated with Lisp during the early part of my career and even decided to write my own interpreter. Even though the interpreter was quite simple at the time, it was sophisticated enough to build some interesting monitoring applications for middleware and system management software I had been developing at the time.

I have mixed feelings about Clojure...

On one hand, the simplicity and power of the language makes it feel natural in expressing problems in a functional way. There is virtually no ceremony required in doing things that often come with strongly-typed languages. Everything is just a simple expression. As an example, part of the porting exercise was turning a Java input stream into a lazy sequence of characters. While much more complicated in the Kotlin port, it was incredibly simple with Clojure.

On the other hand, I found myself fighting the lack of a type system and the strict enforcement of such types that I came to expect from compiled languages. A compiler can eliminate so many classes of problems that simply cannot be discovered in Clojure until runtime or unless more extensive tests are written. Some of that struggle can be attributed to the unfamiliarity of a new language, but nonetheless, I realized the appeal of strong typing and what the compiler can do for you. For large-scale systems, I would almost certainly opt for a strongly-typed compiled language.

## Building

The distributable artifacts for this project are built using [make](https://www.gnu.org/software/make/), which delegates most of its work to [Clojure CLI Tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools). After installing `make` and `clojure`, clone the repository and issue the command `make all` in the root directory of the project. This produces two distributions under the subdirectory `build` named `rpn-clojure-<version>.tar.gz` and `rpn-clojure-<version>.zip`.

Unpacking either assembly produces a directory structure with the following format:

```text
rpn-clojure-<version>/
+ rpnc
+ rpn
+ lib/
  ...
```

For convenience, you might place `rpn-clojure-<version>/rpnc` and `rpn-clojure-<version>/rpn` in your `PATH` or create an alias.

## License

Copyright 2020 David Edwards

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

<http://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
