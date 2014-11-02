#!/bin/bash
erl -compile players
erl -noshell -run players setup -run players start 100 -run players snapshot
