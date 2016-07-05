#!/bin/sh
read word;
echo $word | 
if [[ $word =~ [a-zA-Z] ]]
then
 enchant -d en_US -a
else
 enchant -d ru_RU -a
fi
