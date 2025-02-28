unit HTTPCodes;

{$mode objfpc}{$H+}

interface

type
  THTTPMethod = (hmGet, hmPost, hmPut, hmDelete, hmPatch, hmHead, hmOptions);

const
  // Respuestas informativas
  HTTP_CONTINUE = 100;
  HTTP_SWITCHING_PROTOCOLS = 101;
  HTTP_PROCESSING = 102;

  // Respuestas satisfactorias
  HTTP_OK = 200;
  HTTP_CREATED = 201;
  HTTP_ACCEPTED = 202;
  HTTP_NON_AUTHORITATIVE_INFORMATION = 203;
  HTTP_NO_CONTENT = 204;
  HTTP_RESET_CONTENT = 205;
  HTTP_PARTIAL_CONTENT = 206;

  // Mensajes de redirección
  HTTP_MULTIPLE_CHOICES = 300;
  HTTP_MOVED_PERMANENTLY = 301;
  HTTP_FOUND = 302;
  HTTP_SEE_OTHER = 303;
  HTTP_NOT_MODIFIED = 304;
  HTTP_USE_PROXY = 305;
  HTTP_TEMPORARY_REDIRECT = 307;

  // Respuestas de error del cliente
  HTTP_BAD_REQUEST = 400;
  HTTP_UNAUTHORIZED = 401;
  HTTP_PAYMENT_REQUIRED = 402;
  HTTP_FORBIDDEN = 403;
  HTTP_NOT_FOUND = 404;
  HTTP_METHOD_NOT_ALLOWED = 405;
  HTTP_NOT_ACCEPTABLE = 406;
  HTTP_PROXY_AUTHENTICATION_REQUIRED = 407;
  HTTP_REQUEST_TIMEOUT = 408;
  HTTP_CONFLICT = 409;
  HTTP_GONE = 410;

  // Otros -------------------------------------
  HTTP_TOO_MANY_REQUESTS = 429;

  // Respuestas de error del servidor
  HTTP_INTERNAL_SERVER_ERROR = 500;
  HTTP_NOT_IMPLEMENTED = 501;
  HTTP_BAD_GATEWAY = 502;
  HTTP_SERVICE_UNAVAILABLE = 503;
  HTTP_GATEWAY_TIMEOUT = 504;
  HTTP_HTTP_VERSION_NOT_SUPPORTED = 505;

function GetHTTPStatusMessage(code: Integer; longFormat: Boolean): String;

implementation

function HTTPMethodToStr(Method: THTTPMethod): string;
begin
  case Method of
    hmGet:     Result := 'GET';
    hmPost:    Result := 'POST';
    hmPut:     Result := 'PUT';
    hmDelete:  Result := 'DELETE';
    hmPatch:   Result := 'PATCH';
    hmHead:    Result := 'HEAD';
    hmOptions: Result := 'OPTIONS';
  else
    Result := 'GET'; // Default fallback
  end;
end;

function GetHTTPStatusMessage(code: Integer; longFormat: Boolean): String;
begin
  case code of
    // Respuestas informativas
    HTTP_CONTINUE:
      if longFormat then Result := 'Continuar: El servidor ha recibido los encabezados de la solicitud y el cliente debe proceder a enviar el cuerpo de la solicitud.' else Result := 'Continuar';
    HTTP_SWITCHING_PROTOCOLS:
      if longFormat then Result := 'Cambiando protocolos: El solicitante ha pedido al servidor que cambie de protocolo.' else Result := 'Cambiando protocolos';
    HTTP_PROCESSING:
      if longFormat then Result := 'Procesando: El servidor ha recibido y está procesando la solicitud, pero aún no hay respuesta disponible.' else Result := 'Procesando';

    // Respuestas satisfactorias
    HTTP_OK:
      if longFormat then Result := 'OK: La solicitud se completó con éxito.' else Result := 'OK';
    HTTP_CREATED:
      if longFormat then Result := 'Creado: La solicitud se ha cumplido y ha resultado en la creación de un nuevo recurso.' else Result := 'Creado';
    HTTP_ACCEPTED:
      if longFormat then Result := 'Aceptado: La solicitud ha sido aceptada para su procesamiento, pero el procesamiento no se ha completado.' else Result := 'Aceptado';
    HTTP_NON_AUTHORITATIVE_INFORMATION:
      if longFormat then Result := 'Información no autorizada: La solicitud fue exitosa pero la carga útil adjunta ha sido modificada desde la respuesta 200 (OK) del servidor de origen por un proxy de transformación.' else Result := 'Información no autorizada';
    HTTP_NO_CONTENT:
      if longFormat then Result := 'Sin contenido: El servidor procesó la solicitud con éxito y no está devolviendo ningún contenido.' else Result := 'Sin contenido';
    HTTP_RESET_CONTENT:
      if longFormat then Result := 'Restablecer contenido: El servidor procesó la solicitud con éxito, pero no está devolviendo ningún contenido, y requiere que el solicitante restablezca la vista del documento.' else Result := 'Restablecer contenido';
    HTTP_PARTIAL_CONTENT:
      if longFormat then Result := 'Contenido parcial: El servidor está entregando solo parte del recurso (servicio de bytes) debido a un encabezado de rango enviado por el cliente.' else Result := 'Contenido parcial';

    // Mensajes de redirección -----------------------------------------------------------
    HTTP_MULTIPLE_CHOICES:
      if longFormat then Result := 'Múltiples opciones: Hay varias opciones para el recurso del que el cliente puede elegir.' else Result := 'Múltiples opciones';
    HTTP_MOVED_PERMANENTLY:
      if longFormat then Result := 'Movido permanentemente: El recurso se ha movido permanentemente a una nueva URL.' else Result := 'Movido permanentemente';
    HTTP_FOUND:
      if longFormat then Result := 'Encontrado: El recurso se encontró en una URL diferente.' else Result := 'Encontrado';
    HTTP_SEE_OTHER:
      if longFormat then Result := 'Ver otro: La respuesta a la solicitud se puede encontrar en otra URL utilizando un método GET.' else Result := 'Ver otro';
    HTTP_NOT_MODIFIED:
      if longFormat then Result := 'No modificado: El recurso no se ha modificado desde la versión especificada por los encabezados de solicitud.' else Result := 'No modificado';
    HTTP_USE_PROXY:
      if longFormat then Result := 'Usar proxy: El recurso solicitado solo está disponible a través de un proxy, cuya dirección se proporciona en la respuesta.' else Result := 'Usar proxy';
    HTTP_TEMPORARY_REDIRECT:
      if longFormat then Result := 'Redirección temporal: El recurso reside temporalmente bajo una URL diferente.' else Result := 'Redirección temporal';

    // Respuestas de error del cliente ---------------------------------------------------
    HTTP_BAD_REQUEST:
      if longFormat then Result := 'Solicitud incorrecta: El servidor no pudo entender la solicitud debido a una sintaxis inválida.' else Result := 'Solicitud incorrecta';
    HTTP_UNAUTHORIZED:
      if longFormat then Result := 'No autorizado: El cliente debe autenticarse para obtener la respuesta solicitada.' else Result := 'No autorizado';
    HTTP_PAYMENT_REQUIRED:
      if longFormat then Result := 'Pago requerido: El cliente debe pagar para obtener la respuesta solicitada.' else Result := 'Pago requerido';
    HTTP_FORBIDDEN:
      if longFormat then Result := 'Prohibido: El cliente no tiene derechos de acceso al contenido.' else Result := 'Prohibido';
    HTTP_NOT_FOUND:
      if longFormat then Result := 'No encontrado: El servidor no puede encontrar el recurso solicitado.' else Result := 'No encontrado';
    HTTP_METHOD_NOT_ALLOWED:
      if longFormat then Result := 'Método no permitido: El método de solicitud es conocido por el servidor pero no es compatible con el recurso de destino.' else Result := 'Método no permitido';
    HTTP_NOT_ACCEPTABLE:
      if longFormat then Result := 'No aceptable: El servidor no puede producir una respuesta que coincida con los valores de aceptación definidos en los encabezados de negociación proactiva de contenido de la solicitud.' else Result := 'No aceptable';
    HTTP_PROXY_AUTHENTICATION_REQUIRED:
      if longFormat then Result := 'Autenticación de proxy requerida: El cliente debe autenticarse con un proxy.' else Result := 'Autenticación de proxy requerida';
    HTTP_REQUEST_TIMEOUT:
      if longFormat then Result := 'Tiempo de solicitud agotado: El servidor desea cerrar esta conexión no utilizada.' else Result := 'Tiempo de solicitud agotado';
    HTTP_CONFLICT:
      if longFormat then Result := 'Conflicto: La solicitud entra en conflicto con el estado actual del servidor.' else Result := 'Conflicto';
    HTTP_GONE:
      if longFormat then Result := 'Eliminado: El contenido ha sido eliminado permanentemente del servidor, sin dirección de reenvío.' else Result := 'Eliminado';


    // Otros -----------------------------------------------------------------------
    HTTP_TOO_MANY_REQUESTS:
      if longFormat then Result := 'Demasiadas solicitudes: Se ha sobrepasado el limite de pedidos habilitados en el servidor.' else Result := 'Demasiadas solicitudes';

    // Respuestas de error del servidor --------------------------------------------
    HTTP_INTERNAL_SERVER_ERROR:
      if longFormat then Result := 'Error interno del servidor: El servidor ha encontrado una situación que no sabe cómo manejar.' else Result := 'Error interno del servidor';
    HTTP_NOT_IMPLEMENTED:
      if longFormat then Result := 'No implementado: El método de solicitud no es compatible con el servidor y no puede ser manejado.' else Result := 'No implementado';
    HTTP_BAD_GATEWAY:
      if longFormat then Result := 'Puerta de enlace incorrecta: El servidor, mientras actúa como puerta de enlace o proxy, recibió una respuesta no válida del servidor ascendente.' else Result := 'Puerta de enlace incorrecta';
    HTTP_SERVICE_UNAVAILABLE:
      if longFormat then Result := 'Servicio no disponible: El servidor no está listo para manejar la solicitud.' else Result := 'Servicio no disponible';
    HTTP_GATEWAY_TIMEOUT:
      if longFormat then Result := 'Tiempo de espera de la puerta de enlace: El servidor, mientras actúa como puerta de enlace o proxy, no recibió una respuesta oportuna del servidor ascendente.' else Result := 'Tiempo de espera de la puerta de enlace';
    HTTP_HTTP_VERSION_NOT_SUPPORTED:
      if longFormat then Result := 'Versión HTTP no soportada: La versión HTTP utilizada en la solicitud no es compatible con el servidor.' else Result := 'Versión HTTP no soportada';

    else
      if longFormat then Result := 'Código de estado desconocido: El código de estado proporcionado no es reconocido.' else Result := 'Desconocido';
  end;
end;

end.

