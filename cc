#!/bin/bash
java -Xmx3G -Xss512m -cp ./bin:./lib/scala-library.jar:./lib/kiama_2.10-1.5.0.jar:./lib/dsprofile_2.10-0.2.0.jar choicecalculus.interpreter.CommandLine $1