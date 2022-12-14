set -e
echo "# Tests"
nim r utils.nim
echo "# Data"
nim r step1.nim data.txt
