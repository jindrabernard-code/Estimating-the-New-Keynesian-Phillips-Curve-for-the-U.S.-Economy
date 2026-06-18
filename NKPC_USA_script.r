# ============================================================
# Estimating the New Keynesian Phillips Curve for the U.S. Economy
# Replication and extension of Gali-Gertler (1999)
# Author: Jindrich Bernard
# Masarykova univerzita, 2026
# Data: FRED, 1990-2025
# ============================================================
#
# DESCRIPTION:
# This script estimates forward-looking and hybrid NKPC models for
# the U.S. economy using Generalized Method of Moments (GMM),
# Nonlinear Least Squares (NLS), and Maximum Likelihood (ML) methods
# (with normal and t-distributed errors).
#
# The NKPC takes the form:
#   pi_t = beta * E[pi_{t+1}] + kappa * x_t + epsilon_t   (forward-looking)
# or in hybrid form:
#   pi_t = gamma_f * E[pi_{t+1}] + gamma_b * pi_{t-1} + kappa * x_t
#
# Diagnostics: J-tests, LR and Wald tests of long-run verticality
# Preferred model: Hybrid structural GMM with valid instruments
# ============================================================
#
# NOTE: Please replace this file with the actual NKPC_USA_script (3).r
#       You can upload the file by going to:
#       Add file > Upload files on the repository main page
# ============================================================

# Please upload the actual script file NKPC_USA_script (3).r
