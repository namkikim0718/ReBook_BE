package com.be.rebook.product.repository;

import com.be.rebook.product.dto.ProductRequestDTO;
import com.be.rebook.product.entity.Product;

import java.util.List;

import static com.be.rebook.product.dto.ProductRequestDTO.*;

public interface ProductRepositoryCustom {

    List<Product> findProductsByFilter(ProductFilterDTO productFilterDTO);
}
