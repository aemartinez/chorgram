#!/usr/bin/env python
# -*- coding: utf-8 -*-
# @Time    : 2017/2/28 pm 2:07
# @Author  : Zhao Liu
# @Site    :
# @File    : view.py
# @Software: PyCharm
from django.shortcuts import render,render_to_response
import os
import json
from django.http import HttpResponse
from django.conf import settings
import subprocess
from django.views.decorators.csrf import csrf_exempt
from os import listdir
from os.path import isfile,join
import shutil


BASE_DIR = settings.BASE_DIR

# Jumping into index.html
def index(request):
    return render(request,'index.html')

# Find all templates files and return to web page
def example(request):
    onlyfiles = [f for f in listdir(os.path.join(BASE_DIR, 'chorgram/static/examples'))
                 if isfile(join(os.path.join(BASE_DIR, 'chorgram/static/examples'), f))]
    print onlyfiles
    return render(request, 'pages/example.html', {'filelist': onlyfiles})
# Read file by name
@csrf_exempt
def showdata_ajax(request):
    filename = request.GET.get('filename')
    print filename

    try:
        f = open(os.path.join(BASE_DIR, 'chorgram/static/examples', filename), 'rb')
    except:
        f= open(os.path.join(BASE_DIR, 'chorgram/static/uploadfile', filename), 'rb')
    line = f.readline()
    content = line
    while line:
        print line
        line = f.readline()
        content += line
    f.close()
    return HttpResponse(content)


# jump to upload.html

def upload(request):
    return render(request, 'pages/upload.html')

# jump to input.html
def input(request):
    return render(request, 'pages/input.html')


# jump to template.html
def template(request):
    return render(request, 'pages/template.html')


# ajax post the text area
@csrf_exempt
def textarea_ajax(request):
    textcontent = request.GET.get('textarea')
    cammnads = textcontent.split('\n')
    print len(cammnads)
    filename = 'default.fsa'
    f = open(os.path.join(BASE_DIR, 'chorgram/static/uploadfile', filename), 'wb')
    for i in range(len(cammnads)):
        f.write(str(cammnads[i]))
        f.write('\n')
    f.close()
    return HttpResponse(filename)


# ajax upload file
# if the user uploads the file then get the name,
# else if user inputs commands then generate a fsa file in upload file
@csrf_exempt
def upload_ajax(request):
    if request.method == 'POST':
        file = request.FILES.get('file')
        print(file.name)
        f = open(os.path.join(BASE_DIR, 'chorgram/static/uploadfile', file.name), 'wb')
        for chunk in file.chunks():
            f.write(chunk)
        f.close()
    else:
        file = request.FILES.get('file')
        print (file.name)
        # return HttpResponse({'filename':file.name})
        # return render(request,'pages/upload.html',{'filename':file.name})
    filename = file.name
    json_str = json.dumps(filename)
    return HttpResponse(json_str)
# get dot file by file name and dirName
@csrf_exempt
def get_dot(request):
    filename = request.GET.get("filename")
    dirName = request.GET.get("dirName")
    path = 'chorgram/static/out/' + dirName
    if filename.find("global") != -1:
        path = 'chorgram/static/results/' + dirName
        filename = dirName + '_petrinet_global.dot'
    f = open(os.path.join(BASE_DIR, path, filename), 'rb')
    line = f.readline()
    content = line + '\n'
    while line:
        print line
        line = f.readline()
        content += line
    f.close()
    return HttpResponse(content)
    # path = 'chorgram/static/out/'
    # for root, dirs, files in os.walk(os.path.join(BASE_DIR,path)):
    #     if filename in files:
    #         filePath = os.path.join(root,filename)

# through ajax requests to compute the graphs or debug messages
# according the file which uploaded by user to compute graphs
def get_file(request):
    filename = request.GET.get('filename')
    splitname = filename.split('.', 1)
    print (splitname[0])
    name = filename
    path = 'chorgram/static/out/' + splitname[0]
    cleanMessage = cleanFile(os.path.join(BASE_DIR, path))
    if (cleanMessage == 0):
        print 'file clean done'
    exeMessage = runByShell(name)  # run(name)
    print exeMessage;
    PICS = os.listdir(os.path.join(BASE_DIR, path))
    if (len(PICS) == 1):
        result_list = 1
        return HttpResponse(
            json.dumps(result_list),
            content_type='application/json'
        )
    result_list = PICS
    # filter(lambda x: x.find(".png"),PICS)
    print 'result_list', result_list
    return HttpResponse(
        json.dumps(result_list),
        content_type='application/json'
    )
# Testing according the file to generate dot and return to page
def get_file_return_dot(request):
    filename = request.GET.get('filename')
    splitname = filename.split('.', 1)
    #print (splitname[0])
    name = filename
    outPath = 'chorgram/static/out/' + splitname[0]
    paths = [os.path.join(BASE_DIR, 'chorgram/static/out/' + splitname[0]),
             os.path.join(BASE_DIR, 'chorgram/static/results/' + splitname[0])]

    cleanMessage = cleanFile(paths)
    if (cleanMessage == 0):
        print 'file clean done'
    exeMessage = runByShell(name)  # run(name)
    print exeMessage

    PICS = os.listdir(os.path.join(BASE_DIR, outPath))
    if (len(PICS) == 1):
        result_list = 1
        return HttpResponse(
            json.dumps(result_list),
            content_type='application/json'
        )
    # chorgram/static/out/ no global.dot file , need add
    pathForGlobal = 'chorgram/static/results/' + splitname[0]
    globalDotFile = searchFile(os.path.join(BASE_DIR, pathForGlobal), 'global', '.dot')
    PICS.append(globalDotFile)

    result_list = PICS
    # filter(lambda x: x.find(".png"),PICS)
    # print 'result_list', result_list
    return HttpResponse(
        json.dumps(result_list),
        content_type='application/json'
    )
# Run shell command to use tool
# This method could return exceptions
# If first command has failed to perform, then return 1
# If second command has failed to perform, then return 2
# If all command executed successfully, then return 0
def runByShell(name):
    messages = 0
    command1 = 'cd chorgram\n python cfsm2gg.py -nc -dw 0 -df pdf -dir static/results/ static/uploadfile/'
    operate1 = command1 + name
    splitname = name.split('.', 1)
    command2 = 'cd chorgram\n python cfsm2gg.py -df png -b 1 --dot "Nshape=parallelogram" --dot "Gnodesep=.5" -dir static/out \
                    -p "w *" -tp "'+splitname[0]+' * *" -cp "* * '+splitname[0]+'" static/uploadfile/'
    operate2 = command2+name;
    try:
       subprocess.call(operate1,shell=True)

    except:
        messages = 1
        print messages
        print 'operate1 failed!!!'
        return messages
    try:
        subprocess.call(operate2, shell=True)
    except:
        messages = 2
        print messages
        print 'operate2 failed!!!'
        return messages
    return messages

# clean file according path
def cleanFile(paths):
    exeMessage=0
    for p in paths:
        try:
            shutil.rmtree(p)
        except:
            return 1
        #print 'clean done!'
    return exeMessage;

# Get all file from path
def getAllFIleFromPath(path):
    onlyfiles = [f for f in listdir(path)
                 if isfile(path, f)]
    return onlyfiles

# According file name and path search file
# the file name include text
def searchFile(path, text, ext):
    try:
        files = os.listdir(path)
        if files is None:
            return None
        for f in files:
            file = os.path.join(path, f)
            if os.path.isdir(file):
                print file
                searchFile(file, text)
            elif os.path.isfile(file):
                temp = os.path.splitext(file)[0]
                if (temp.find(text) != -1):
                    temp = os.path.splitext(file)[1]
                    if (temp == ext):
                        return os.path.split(file)[1]
    except Exception:
        print u'Search file failed!!'
