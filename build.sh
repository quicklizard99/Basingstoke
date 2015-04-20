VERSION=`cat DESCRIPTION  | grep Version: | sed s/.*:\ //g`
echo Building Basingtsoke ${VERSION} using `R --version | head -1`
# TODO check --as-cran should be run by R-devel
rm -rf build  && \
    mkdir build && \
    cd build && \
    R CMD build --compact-vignettes=gs .. && \
    R CMD check Basingstoke_${VERSION}.tar.gz && \
    R CMD INSTALL --build --html Basingstoke_${VERSION}.tar.gz
