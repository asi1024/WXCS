#! /bin/sh

echo "hue is running..."

TEMP="curl -s -X PUT http://aizawa/api/newdeveloper/groups/1/action -d"

$TEMP '{"on":true, "sat":254, "bri":250,"hue":40000}'
$TEMP '{"on":true}'
#$TEMP '{"alert":"select"}'
sleep 0.5
$TEMP '{"on":true, "sat":254, "bri":254,"hue":30000}'
$TEMP '{"on":true}'
