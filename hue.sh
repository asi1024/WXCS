#! /bin/sh

if test $# -ne 2 ; then
    echo "error~~~~~~~~"
    exit
fi

HUE="curl -s -X PUT http://aizawa/api/newdeveloper/groups/1/action -d"

if test $1 = "1" ; then
    $HUE '{"on":true, "sat":255, "bri":255,"hue":42000}'
else
    $HUE '{"on":true, "sat":255, "bri":255,"hue":12000}'
fi

$HUE '{"on":true}'
sleep 0.7
$HUE '{"on":false}'

sleep 0.3

$HUE '{"on":true, "sat":255, "bri":255,"hue":'$2'}'

$HUE '{"on":true}'
sleep 0.3
$HUE '{"on":false}'
