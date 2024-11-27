#!/bin/bash

# Exit on error
set -e

# Help message
show_help() {
    echo "Usage: ./build.sh [clean]"
    echo "  clean    Clean build directory before building"
    exit 0
}

# Process arguments
if [ "$1" == "-h" ] || [ "$1" == "--help" ]; then
    show_help
fi

# Clean if requested
if [ "$1" == "clean" ]; then
    echo "Cleaning build directory..."
    rm -rf build
fi

# Create and enter build directory
mkdir -p build
cd build

# Configure with CMake
echo "Configuring with CMake..."
cmake ..

# Build
echo "Building..."
make -j$(sysctl -n hw.ncpu)

# Run tests
echo "Running tests..."
ctest --output-on-failure

# Return to original directory
cd ..

echo "Build complete!"