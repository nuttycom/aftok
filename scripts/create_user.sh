#!/bin/bash

curl -v -H "Content-Type: application/json" -d '{"username":"nuttycom", "password":"kjntest", "email":"kris@quixoticcompany.com", "btcAddr":"1KamUn1BaRMd2HwikyQWGTdUvfPScg9QA5"}' http://localhost:8000/register

