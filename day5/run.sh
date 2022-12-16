set -e
echo "# Tests"
nim r utils.nim
echo "# Step 1"
nim r step1.nim data.txt
echo "# Step 2"
nim r step2.nim data.txt
