
## Generating test certificates

### CA

#### Configuration (ca.conf)

[ req ]
encrypt_key = no
default_md = sha256
prompt = no
utf8 = yes
distinguished_name = my_req_distinguished_name
req_extensions = my_extensions

[ my_req_distinguished_name ]
C = US
ST = California
L = San Francisco
O  = Piaf
CN = CA

[ my_extensions ]
keyUsage=critical, digitalSignature, keyEncipherment
basicConstraints=critical,CA:TRUE
extendedKeyUsage=critical,serverAuth
subjectKeyIdentifier = hash

#### Create the CA certificate

openssl genpkey -algorithm EC -out ca.key -pkeyopt ec_paramgen_curve:secp384r1 -pkeyopt ec_param_enc:named_curve
openssl req -new -x509 -days 3650 -config ca.conf -out ca.pem -key ca.key

### Server cert

#### Configuration (server.conf)

[ req ]
encrypt_key = no
default_md = sha256
prompt = no
utf8 = yes
distinguished_name = my_req_distinguished_name

[ my_req_distinguished_name ]
C = US
ST = California
L = San Francisco
O  = Piaf
CN = localhost

#### Extensions (ssl-extensions-x509.conf)

[v3_ca]
keyUsage=critical, digitalSignature, keyEncipherment
basicConstraints=critical,CA:FALSE
extendedKeyUsage=critical,serverAuth
subjectKeyIdentifier = hash

#### Create the server certificate

openssl genpkey -algorithm EC -out server.key -pkeyopt ec_paramgen_curve:secp384r1 -pkeyopt ec_param_enc:named_curve
openssl req -new -config server.conf -out server.csr -key server.key
openssl x509 -req -in server.csr -CA ca.pem -CAkey ca.key -CAcreateserial -out server.pem -days 3650 -extensions v3_ca -extfile ./ssl-extensions-x509.conf

### Client cert

#### Configuration (client.conf)

[ req ]
encrypt_key = no
default_md = sha256
prompt = no
utf8 = yes
distinguished_name = my_req_distinguished_name

[ my_req_distinguished_name ]
C = US
ST = California
L = San Francisco
O  = Piaf
CN = localhost

#### Extensions (ssl-extensions-x509.conf)

[v3_ca]
keyUsage=critical, digitalSignature, keyEncipherment
basicConstraints=critical,CA:FALSE
extendedKeyUsage=critical,clientAuth
subjectKeyIdentifier = hash