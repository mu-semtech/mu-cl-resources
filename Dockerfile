FROM madnificent/lisp-webservice:0.6.0

COPY . /app
COPY load-config.sh /load-config.sh
COPY startup-resources.sh /startup-resources.sh

ENV BOOT=mu-cl-resources
RUN mkdir -p /config
RUN sbcl --load /usr/src/load.lisp

CMD ["/startup-resources.sh"]
