# RPN Compiler and Interpreter (Clojure version)

A Clojure implementation of the RPN compiler and interpreter. See the original [RPN project](https://github.com/davidledwards/rpn) for documentation.

## Comments

TODO

## Building

The distributable artifacts for this project are built using [make](https://www.gnu.org/software/make/), which delegates most of its work to [Clojure CLI Tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools). After installing `make` and `clojure`, clone the repository and issue the command `make all` in the root directory of the project. This produces two distributions under the subdirectory `build` named `rpn-clojure-<version>.tar.gz` and `rpn-clojure-<version>.zip`.

Unpacking either assembly produces a directory structure with the following format:

```text
rpn-<version>/
+ rpnc
+ rpn
+ lib/
  ...
```

For convenience, you might place `rpn-<version>/rpnc` and `rpn-<version>/rpn` in your `PATH` or create an alias.

## License

Copyright 2020 David Edwards

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

<http://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
