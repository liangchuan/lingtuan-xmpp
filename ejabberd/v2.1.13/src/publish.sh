#./configure --prefix=/opt/ejabberd/runtime --enable-odbc --enable-user=root
make clean
make
rm -rf /app/ejabberd/*
make install
rm -rf *.beam

#cp rfc/*.* /app/ejabberd/lib/ejabberd/ebin/
echo "========================================="
echo "= OK"
echo "========================================="
