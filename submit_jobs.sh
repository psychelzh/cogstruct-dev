#!/usr/bin/env bash

for file in _scripts_hddm/retest_*.R; do
  qsub -v SCRIPT=$file ./_scripts_hddm/hddm.qsub
done
