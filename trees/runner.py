# Note: This script is meant for testing purposes only.
# Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
import subprocess
import argparse
import sys
import os

current_task = 0

def pipe (arg, output='', mode='w'):
    """ Run a script and pipe the std to given output """
    if output == '':
        popen = subprocess.Popen(arg)
        popen.communicate()
    else:
        with open(output, mode) as f:
            popen = subprocess.Popen(arg, stdout=f)
            popen.communicate()

def log(message):
    sys.stderr.write(message + '\n')

def progress(total, status=''):
    """ see: https://gist.github.com/vladignatyev/06860ec2040cb497f0f3 """
    global current_task
    current_task += 1
    count = current_task

    bar_len = 60
    filled_len = int(round(bar_len * count / float(total)))

    percents = round(100.0 * count / float(total), 1)
    bar = '=' * filled_len + '-' * (bar_len - filled_len)

    sys.stdout.write('[%s] %s%s ...%s\r' % (bar, percents, '%', status))
    sys.stdout.flush()  # As suggested by Rom Ruben

# parse input
parser = argparse.ArgumentParser(description='Runner')
parser.add_argument('-m', '--mode', dest='mode',
        required=True, help='Runner mode (either compile, generate or clean).')

args = parser.parse_args()
if args.mode == 'compile':

    tasks = 8

    os.chdir('tree-visualiser/')
    progress(tasks,"Tree generator compilation... May take some time...")
    pipe(['cabal','install'])
    os.chdir('..')

    progress(tasks,"Generating bb specification...")
    pipe(['python','trees.py'])

    progress(tasks,"Generating paganini specification...")
    pipe(['bb','spec', '-f','-i','output.txt'],'paganini.pg')

    progress(tasks,"Calculating tuning parameters...")
    pipe(['medulla','-i','paganini.pg','-p','1.0e-20'],'bb.param')

    progress(tasks,"Sampler generation...")
    pipe(['bb','compile','-f','-t','bb.param','-i','output.txt'],'tree-generator/src/Sampler.hs')

    progress(tasks,"Drop bb generator parameters from output.txt...")
    pipe(['tail','-n','+4','output.txt'],'output_drop.txt')

    progress(tasks,"Generating string representation functions...")
    pipe(['python','smyt.py','output_drop.txt'],'tree-generator/src/Sampler.hs','a')

    os.chdir('tree-generator/')
    progress(tasks,"Compilation... May take some time...")
    pipe(['cabal','install','--overwrite-policy=always'])
    os.chdir('..')

    progress(tasks,"Done.")
    exit(0)

if args.mode == "generate":

    (lb, ub) = (500, 550)
    log("Generating a random tree of size [" + str(lb) + ", " + str(ub) + "]...")
    pipe(['tree-generator',str(lb),str(ub)], 'tree.in')

    log("Generating a graphviz dot representation...")
    pipe(['tree-visualiser','tree.in'], 'tree.dot')

    log("Generating a EPS file...")
    pipe(['dot','-Teps','tree.dot'], 'tree.eps')

    progress(tasks, 'Generating PNG file...')
    pipe(['convert','-density','300','tree.eps','tree.png'])
    log("Done.")
    exit(0)

if args.mode == "clean":

    pipe(['rm','bb.param'])
    pipe(['rm','paganini.pg'])
    pipe(['rm','output.txt'])
    pipe(['rm','output_drop.txt'])
    pipe(['rm','tree.dot'])
    pipe(['rm','tree.in'])
    pipe(['rm','tree.eps'])
    pipe(['rm','tree.png'])
    exit(0)

log("Illegal runner mode: expected compile, generate or clean.")
exit(1)
