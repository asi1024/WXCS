#! /bin/sh

if test $# -ne 1 ; then
    echo "error~~~~~~~~"
    exit
fi

TEMP="curl -s -X PUT http://aizawa/api/newdeveloper/groups/1/action -d"

if test $1 = "1" ; then
    $TEMP '{"on":true, "sat":255, "bri":255,"hue":42000}'
else
    $TEMP '{"on":true, "sat":255, "bri":255,"hue":12000}'
fi

$TEMP '{"on":true}'
#$TEMP '{"alert":"select"}'
sleep 0.5
#$TEMP '{"on":true, "sat":0, "bri":255,"hue":0}'
$TEMP '{"on":false}'
