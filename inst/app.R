# app.R

# Instala dependencias necesarias 
pkgload::load_all()

# Carga tu app desde el paquete y llama a la base de datos
geocovidapp::run_app()
