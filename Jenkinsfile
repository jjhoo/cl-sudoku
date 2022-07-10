node {
    checkout scm
    def userId = sh(script: "id -u ${USER}", returnStdout: true).trim()
    def customImage = docker.build("build-cl-sudoku:${env.BUILD_ID}", "--build-arg JENKINS_UID=${userId} -f .jenkins/docker/Dockerfile .jenkins/docker")

    sh 'mkdir -p ${WORKSPACE_TMP}/quicklisp'
    sh 'mkdir -p ${WORKSPACE_TMP}/.cache/common-lisp'

    cache(maxCacheSize: 250, defaultBranch: 'master', caches: [
        [$class: 'ArbitraryFileCache', path: "${env.WORKSPACE_TMP}/quicklisp", cacheValidityDecidingFile: 'cl-sudoku.asd', compressionMethod: 'TARGZ'],
        [$class: 'ArbitraryFileCache', path: "${env.WORKSPACE_TMP}/.cache/common-lisp", cacheValidityDecidingFile: 'cl-sudoku.asd', compressionMethod: 'TARGZ']
    ]) {
        customImage.inside('-v ${WORKSPACE_TMP}/quicklisp:/home/jenkins/quicklisp -v ${WORKSPACE_TMP}/.cache/common-lisp:/home/jenkins/.cache/common-lisp') {
            stage('Setup quicklisp') {
               sh("""
               if [ -f ~/quicklisp/setup.lisp ]; then
                   echo "Quicklisp already installed"
               else
                   echo "Quicklisp not installed"
                   sbcl --script /tmp/install-quicklisp.lisp
               fi
               """)
               sh 'mkdir ~/common-lisp'
               sh 'ln -s ${WORKSPACE} ~/common-lisp/cl-sudoku'
            }
            stage('Build') {
               sh 'sbcl --eval \'(ql:quickload "cl-sudoku")\''
            }
            stage('Build tests') {
               sh 'sbcl --eval \'(ql:quickload "cl-sudoku/tests")\''
            }
            stage('Run tests') {
               sh 'sbcl --eval \'(asdf:test-system "cl-sudoku")\''
            }
        }
    }
}
