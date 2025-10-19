import pandas as pd
from sqlalchemy import create_engine, text
from dotenv import load_dotenv
import os

load_dotenv()

RDS_HOST = os.getenv('RDS_HOST')
RDS_USER = os.getenv('RDS_USER')
RDS_PASSWORD = os.getenv('RDS_PASSWORD')
NUEVA_BD = os.getenv('NUEVA_BD')

engine_bd = create_engine(f"mysql+mysqlconnector://{RDS_USER}:{RDS_PASSWORD}@{RDS_HOST}:3306/{NUEVA_BD}")

def crear_esquema():
    with engine_bd.connect() as conn:
        conn.execute(text(f"CREATE DATABASE IF NOT EXISTS {NUEVA_BD}"))

def crear_tabla():
    with engine_bd.connect() as conn:
        conn.execute(text("""
        CREATE TABLE IF NOT EXISTS smart_alerts (
        cod_comercio VARCHAR(255),
        cod_situacion_comercio VARCHAR(255),
        desc_situacion_comercio VARCHAR(255),
        tipo_documento VARCHAR(255),
        subtipo_documento VARCHAR(255),
        moneda_comercio VARCHAR(255),
        cod_giro_comercio VARCHAR(255),
        nom_giro_comercio VARCHAR(255),
        com_visa_credito FLOAT,
        com_visa_debito FLOAT,
        departamento_comercio VARCHAR(255),
        provincia_comercio VARCHAR(255),
        distrito_comercio VARCHAR(255),
        fecha_apertura_comercio DATE,
        periodo_apertura_comercio DATE,
        nom_banco_pago_comercio VARCHAR(255),
        fecha_ult_compra DATE,
        cant_terminal_pos_mcp_total INTEGER,
        Estado VARCHAR(255),
        TipoFacturacion VARCHAR(255),
        NroTrab VARCHAR(255),
        flag_churn INTEGER,
        cant_trx_m0 INTEGER,
        dias_mes_trx_m0 INTEGER,
        importe_trx_m0 FLOAT,
        cant_trx_m1 INTEGER,
        dias_mes_trx_m1 INTEGER,
        importe_trx_m1 FLOAT,
        cant_trx_m2 INTEGER,
        dias_mes_trx_m2 INTEGER,
        importe_trx_m2 FLOAT,
        cant_trx_m3 INTEGER,
        dias_mes_trx_m3 INTEGER,
        importe_trx_m3 FLOAT,
        cant_trx_m4 INTEGER,
        dias_mes_trx_m4 INTEGER,
        importe_trx_m4 FLOAT,
        cant_trx_m5 INTEGER,
        dias_mes_trx_m5 INTEGER,
        importe_trx_m5 FLOAT,
        cant_trx_m6 INTEGER,
        dias_mes_trx_m6 INTEGER,
        importe_trx_m6 FLOAT
        )
    """))

def subir_datos():
    chunk_size = 10000
    total_rows = 0

    for chunk in pd.read_csv('C:/Users/HP/Desktop/Gestion Datos/datos/demo_proyecto_smart_alerts.csv', sep=';', encoding='utf-8', chunksize=chunk_size):
        chunk.to_sql('smart_alerts', engine_bd, if_exists='append', index=False)
        total_rows += len(chunk)
        print(f"Procesadas {total_rows} filas...")

    print(f"Importación completada")

def main():
    crear_esquema()
    crear_tabla()
    subir_datos()

if __name__ == "__main__":
    main()