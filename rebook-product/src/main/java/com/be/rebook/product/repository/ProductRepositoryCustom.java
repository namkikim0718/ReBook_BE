package com.be.rebook.product.repository;

import com.be.rebook.product.dto.ProductRequestDTO;
import com.be.rebook.product.entity.Product;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

import static com.be.rebook.product.dto.ProductRequestDTO.*;

public interface ProductRepositoryCustom {

    Page<Product> findProductsByFilter(ProductRequestDTO.ProductFilterDTO productFilterDTO, Pageable pageable);
}
