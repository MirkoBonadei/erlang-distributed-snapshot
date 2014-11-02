#!/bin/bash
erl -compile players
erl -noshell -run players setup -run players start 200 -run players snapshot
