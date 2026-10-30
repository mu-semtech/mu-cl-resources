FROM madnificent/lisp-webservice:0.8.0

COPY . /app
COPY load-config.sh /load-config.sh
COPY startup-resources.sh /startup-resources.sh

ENV BOOT=mu-cl-resources
RUN sbcl --load /usr/src/load.lisp

# Support running as an arbitrary UID in group 0 (OpenShift restricted SCC)
RUN mkdir -p /config \
 && chmod 755 /usr/local/bin/sbcl \
 && chgrp -R 0 /root /app /config \
 && chmod -R g=u /root /app /config /etc/passwd
ENV HOME=/root

CMD ["/startup-resources.sh"]
