#!/bin/bash

#SBATCH --partition=geo

set -e

IMAGE="ama_container.sif"
DEF="ama_container2.def"

echo "Building Apptainer image..."
echo "Definition: ${DEF}"
echo "Output:     ${IMAGE}"

apptainer build --fakeroot "${IMAGE}" "${DEF}"

echo "Container successfully built:"
ls -lh "${IMAGE}"



