FROM debian:trixie-slim

RUN apt-get update && apt-get install -y curl python-is-python3 racket
RUN curl -o install.sh https://sdk.cloud.google.com && chmod +x install.sh
RUN ./install.sh --disable-prompts --install-dir=/usr/lib/
RUN ln -sf /usr/lib/google-cloud-sdk/bin/* /usr/bin/
ADD marv.tgz /usr/lib/marv/
RUN raco pkg install --deps search-auto --no-docs --batch /usr/lib/marv/
RUN raco make /usr/lib/marv/types/gcp/*.mrv
COPY --chmod=755 marv.sh /usr/bin/marv
# RUN raco exe -o /usr/local/bin/marv command.rkt