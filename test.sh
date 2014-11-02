#!/bin/bash
erl -compile players
erl -noshell -run players setup -run players start 20 -run players snapshot
