# Asynchronous Circuit Design Tools
![GitHub License](https://img.shields.io/github/license/IntelLabs/async-toolkit)
[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/IntelLabs/async-toolkit/badge)](https://scorecard.dev/viewer/?uri=github.com/IntelLabs/async-toolkit)

This repository contains a suite of CAD (Computer-Aided Design) tools focused on the design, simulation, and analysis of **asynchronous circuits**. The software is structured into two major components:

## 📦 Components

### 🔹 `m3utils`
A collection of tools designed for digital asynchronous circuit workflows, including:
- Digital design and design management  
- Optimization  
- Design evaluation  
- Verification and validation  

### 🔹 `aspice`
A toolkit specializing in:
- Analog circuit simulation  
- Debugging asynchronous analog behavior  

### 🔹 `jtools`
A toolkit consisting of mainly Java tools containing:
- CSP parsing tools
- CAST parsing tools
- Examples and common setup files for the above

## 🖥️ Usage Model

These tools are intended to be used **stand-alone** within an **interactive Unix-based engineering environment**, operating on **pre-validated user data**—a standard practice in hardware design workflows.

They are **not designed as plug-in components for larger software infrastructures**, although certain modules may be configured by end users to interface with select commercial CAD tools.
